import Distribution.Simple (defaultMainWithHooks,
                            simpleUserHooks,
                            buildHook)
import Distribution.Simple.UserHooks (UserHooks)
import Distribution.PackageDescription as PD (PackageDescription(..),
                                              BuildInfo(..),
                                              Library(..))
import Distribution.ModuleName (ModuleName, components)
import Distribution.Simple.Utils (findFileWithExtension,
                                  die,
                                  withUTF8FileContents)
import Data.List (intercalate)
import Distribution.Text (simpleParse)
import System.FilePath (joinPath)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = defaultMainWithHooks userHooks

userHooks :: UserHooks
userHooks = simpleUserHooks {
    buildHook = \pd lbi uh bf -> fixDeps pd >>= \pd' ->
     buildHook simpleUserHooks pd' lbi uh bf
  }

-- Given all files of the package, find those that end in .chs and extract the
-- .chs files they depend upon. Then return the PackageDescription with these
-- files rearranged so that they are built in a sequence that files that are
-- needed by other files are built first.
fixDeps :: PackageDescription -> IO PackageDescription
fixDeps pd@PD.PackageDescription {
          PD.library = Just lib@PD.Library {
            PD.exposedModules = expMods',
            PD.libBuildInfo = bi@PD.BuildInfo {
              PD.hsSourceDirs = srcDirs,
              PD.otherModules = othMods'
            }}} = do
  let findModule m = findFileWithExtension [".chs.pp",".chs"] srcDirs
                       (joinPath (components m))
  mExpFiles <- mapM findModule expMods'
  mOthFiles <- mapM findModule othMods'

  -- tag all exposed files with True so we throw an error if we need to build
  -- an exposed module before an internal modules (we cannot express this)
  let modDeps' = zipWith (ModDep True []) expMods' mExpFiles ++
                 zipWith (ModDep False []) othMods' mOthFiles
  modDeps <- mapM extractDeps modDeps'
  let (expMods, othMods) = span mdExposed $ sortTopological modDeps
      badOther = map (fromMaybe "<no file>" . mdLocation) $
                 filter (not . mdExposed) expMods
  unless (null badOther) $
    die ("internal chs modules "++intercalate "," badOther++
         " depend on exposed chs modules; cabal needs to build internal modules first")
  return pd { PD.library = Just lib {
    PD.exposedModules = map mdOriginal expMods,
    PD.libBuildInfo = bi { PD.otherModules = map mdOriginal othMods }
  }}

fixDeps _ = undefined

data ModDep = ModDep {
  mdExposed :: Bool,
  mdRequires :: [ModuleName],
  mdOriginal :: ModuleName,
  mdLocation :: Maybe FilePath
}

instance Show ModDep where
  show x = show (mdLocation x)

instance Eq ModDep where
  ModDep { mdOriginal = m1 } == ModDep { mdOriginal = m2 } = m1==m2
instance Ord ModDep where
  compare ModDep { mdOriginal = m1 } ModDep { mdOriginal = m2 } = compare m1 m2

-- Extract the dependencies of this file. This is intentionally rather naive as it
-- ignores CPP conditionals. We just require everything which means that the
-- existance of a .chs module may not depend on some CPP condition.
extractDeps :: ModDep -> IO ModDep
extractDeps md@ModDep { mdLocation = Nothing } = return md
extractDeps md@ModDep { mdLocation = Just f } = withUTF8FileContents f $ \con -> do
  let findImports acc (('{':'#':xs):xxs) = case (dropWhile (' ' ==) xs) of
        ('i':'m':'p':'o':'r':'t':' ':ys) ->
          case simpleParse (takeWhile ('#' /=) ys) of
            Just m -> findImports (m:acc) xxs
            Nothing -> die ("cannot parse chs import in "++f++":\n"++
                            "offending line is {#"++xs)
         -- no more imports after the first non-import hook
        _ -> return acc
      findImports acc (_:xxs) = findImports acc xxs
      findImports acc [] = return acc
  mods <- findImports [] (lines con)
  return md { mdRequires = mods }

-- Find a total order of the set of modules that are partially sorted by their
-- dependencies on each other. The function returns the sorted list of modules
-- together with a list of modules that are required but not supplied by this
-- in the input set of modules.
sortTopological :: [ModDep] -> [ModDep]
sortTopological ms = reverse $ fst $ foldl visit ([], S.empty) (map mdOriginal ms)
  where
  set = M.fromList (map (\m -> (mdOriginal m, m)) ms)
  visit (out,visited) m
    | m `S.member` visited = (out,visited)
    | otherwise = case m `M.lookup` set of
        Nothing -> (out, m `S.insert` visited)
        Just md -> (md:out', visited')
          where
            (out',visited') = foldl visit (out, m `S.insert` visited) (mdRequires md)
