module Main where

import System.Environment
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeFileName, takeExtension)
import Text.Printf (printf)
import Control.Monad (filterM, liftM, liftM2, (=<<))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf)

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Pretty (content, document)
import Text.XML.HaXml.Xml2Haskell (attr2str)

data Project = Project
  {
    name         :: String,
    dependencies :: [String],
    libraries    :: [String]
  } deriving (Show, Eq)

main = do
  args <- getArgs
  mapM readProject args >>= putStr . dotGraph . reduceDependencies

-- reduce dependencies by removing direct dependencies on indirectly
-- imported projects. e.g. a -> [b, c] becomes a -> [b] if b -> [c].

reduceDependencies :: [Project] -> [Project]
reduceDependencies projects = map reduceProject projects
  where
    reduceProject (Project name deps libraries) = 
      Project name (reduce deps) libraries

    reduce :: [String] -> [String]
    reduce deps = filter (not . reachableIndirect deps) deps

    -- can we reach target by following from a set of targets?
    reachableIndirect :: [String] -> String -> Bool
    reachableIndirect targets target = 
      target `elem` (transitiveDeps $ filter ((/=) target) targets)

    transitiveDeps :: [String] -> [String]
    transitiveDeps []       = []
    transitiveDeps projects =
      projects ++ (transitiveDeps $ concatMap dependenciesFor projects)

    -- project name -> dependencies ([] if project unknown)
    dependenciesFor :: String -> [String]
    dependenciesFor name =
      case Map.lookup name mapNameToProject of
        Just project -> dependencies project
        Nothing      -> []

    mapNameToProject :: Map String Project
    mapNameToProject = Map.fromList [(name p, p) | p <- projects]

-- StringBuilder str = new StringBuilder ();
-- str.apppend (header);
-- for (source : projects)
-- {
--   for (target : source.dependencies)
--   {
--     str.append ("\"").append (source);
--     str.append ("\" -> \"").append (target).append ("\";\n");
--   }
-- }

-- str.append (footer);

-- return str.toString ();

-- generate a graph for input to Graphviz's "dot" program
dotGraph :: [Project] -> String
dotGraph projects = header ++ projectGraph "" ++ footer
  where
    header = "digraph project_vis \n" ++ 
             "{ratio=compress;size=\"7.5,10\";" ++
             "node[fontsize=24];node[fontname=Helvetica];\n\n"
    footer = "\n}\n"

    projectGraph :: ShowS
    projectGraph =
      showString "node [shape=ellipse];\n" .
      (showList $ map projectEdges projects) .
      showString "node [shape=box,style=filled];\n" .
      (showList $ map libraryEdges projects) .
      showString "{rank=sink\n" ++
      (showList $ map libraryLabel allLibraries) ++
      showString "}\n"

--      concatMap licenseEdges projects ++
  
--       "subgraph cluster0 {label=Livespaces;rank=min\n" ++ 
--         (join . filter ("livespace." `isPrefixOf`) . map name) projects ++
--       "}\n" ++

--       "subgraph cluster1 {label=DFC;\n" ++ 
--         (join . filter ("dfc." `isPrefixOf`) . map name) projects ++
--       "}\n" ++
 
--       "subgraph cluster2 {label=Libraries;rank=source\n" ++ 
--         (join . concatMap libraries) projects ++
--       "}\n"

--       "{rank=same;\n" ++ 
--         (join $ concatMap libraries projects) ++
--       "}\n"

    projectEdges p = dotEdges (name p) (dependencies p)
   
    libraryEdges p = dotEdges (name p) (libraries p)

    -- generate a label for a library, including license
    libraryLabel library = 
      let license = licenseFor library in
      "\"" ++ library ++ "\"[label=\"" ++ library ++ 
      (if license == "???" then "" else "\\n" ++ license) ++ 
      "\"];\n"

    allLibraries :: [String]
    allLibraries = concatMap libraries projects

--     licenseEdges p = 
--       concat [dotEdge lib (licenseFor lib) | lib <- libraries p]

--      foldr (\lib s -> dotEdge lib (licenseFor lib) s) "" (libraries p)

    -- join = intercalate "; " . map (\s -> "\"" ++ s ++ "\"")

    dotEdges :: String -> [String] -> ShowS
    dotEdges source targets = foldr (.) id $ map (dotEdge source) targets
--    dotEdges source targets = foldr ($) "" $ map (dotEdge source) targets
--     dotEdges source targets = 
--       foldl (\target s -> dotEdge source target s) "" targets

    dotEdge :: String -> String -> ShowS
    dotEdge source target = 
--      "\"" ++ source ++ "\" -> \"" ++ target ++ "\";\n"
      ('"' :) . showString source . showString "\" -> \"" . showString target . showString "\";\n"

    licenseFor library = 
      Map.findWithDefault "???" library mapLibraryToLicense

    mapLibraryToLicense = Map.fromList [
      ("RXTXcomm.jar", "LGPL 3"),
      ("TalkingHead.jar", "???"),
      ("activation.jar", "???"),
      ("ant.jar", "Apache 2"),
      ("avis-client.jar", "LGPL 3"),
      ("avisd.jar", "GPL 3 (exemption)"),
      ("axis-ant.jar", "Apache 2"),
      ("axis.jar", "Apache 2"),
      ("batik-awt-util.jar", "Apache 2"),
      ("batik-bridge.jar", "Apache 2"),
      ("batik-css.jar", "Apache 2"),
      ("batik-dom.jar", "Apache 2"),
      ("batik-ext.jar", "Apache 2"),
      ("batik-extension.jar", "Apache 2"),
      ("batik-gui-util.jar", "Apache 2"),
      ("batik-gvt.jar", "Apache 2"),
      ("batik-parser.jar", "Apache 2"),
      ("batik-script.jar", "Apache 2"),
      ("batik-svg-dom.jar", "Apache 2"),
      ("batik-svggen.jar", "Apache 2"),
      ("batik-swing.jar", "Apache 2"),
      ("batik-transcoder.jar", "Apache 2"),
      ("batik-util.jar", "Apache 2"),
      ("batik-xml.jar", "Apache 2"),
      ("cfparse.jar", "Apache 2"),
      ("cm_api-1.0.1.jar", "???"),
      ("com.ibm.icu_3.4.5.20061213.jar", "Custom Permissive"),
      ("commons-codec-1.3.jar", "Apache 2"),
      ("commons-collections-3.1.jar", "Apache 2"),
      ("commons-discovery-0.2.jar", "Apache 2"),
      ("commons-httpclient-3.0-rc3.jar", "Apache 2"),
      ("commons-logging-1.0.4.jar", "Apache 2"),
      ("commons-logging-api.jar", "Apache 2"),
      ("commons-logging.jar", "Apache 2"),
      ("concurrent-1.3.4.jar", "Apache 2"),
      ("derby-10.2.1.6.jar", "Apache 2"),
      ("FontBox-0.1.0.jar", "BSD"),
      ("framework.jar", "BSD Style"),
      ("freetts.jar", "BSD Style"),
      ("http_api-1.1.0.jar", "BSD Style"),
      ("jackrabbit-api-1.3.jar", "Apache 2"),
      ("jackrabbit-core-1.3.jar", "Apache 2"),
      ("jackrabbit-jcr-commons-1.3.jar", "Apache 2"),
      ("jackrabbit-jcr-rmi-1.3.jar", "Apache 2"),
      ("jackrabbit-jcr-server-1.3.jar", "Apache 2"),
      ("jackrabbit-text-extractors-1.3.jar", "Apache 2"),
      ("jackrabbit-webdav-1.3.jar", "Apache 2"),
      ("jakarta-oro-2.0.6.jar", "Apache 2"),
      ("jaxen-core.jar", "Apache-style"),
      ("jaxen-jdom.jar", "Apache-style"),
      ("jaxrpc.jar", "CDDL"),
      ("jbcl.jar", "Proprietary"),
      ("jcr-1.0.jar", "???"),
      ("jdom.jar", "Apache 2"),
      ("jetty-6.1.4.jar", "Apache 2"),
      ("jetty-util-6.1.4.jar", "Apache 2"),
      ("jmf.jar", "Sun proprietary"),
      ("jsapi.jar", "Apache-style"),
      ("jsp-api.jar", "???"),
      ("junit.jar", "CPL 1"),
      ("kbml-2.x.jar", "BSD Style"),
      ("kunststoff.jar", "???"),
      ("log4j-1.2.13.jar", "Apache 2"),
      ("lucene-core-2.0.0.jar", "Apache 2"),
      ("mailapi.jar", "???"),
      ("org.eclipse.core.commands.jar", "EPL"),
      ("org.eclipse.core.runtime.jar", "EPL"),
      ("org.eclipse.equinox.common.jar", "EPL"),
      ("org.eclipse.jface.jar", "EPL"),
      ("org.eclipse.ui.forms_3.2.0.v20060602.jar", "EPL"),
      ("resolver.jar", "???"),
      ("saaj.jar", "GPL 2 + classpath"),
      ("saxpath.jar", "???"),
      ("servlet-api-2.5-6.1.4.jar", "???"),
      ("servlet-api.jar", "???"),
      ("slf4j-api-1.3.0.jar", "Apache 2"),
      ("slf4j-api-1.3.0.jar", "Apache 2"),
      ("slf4j-log4j12-1.3.0.jar", "Apache 2"),
      ("swt.jar", "EPL"),
      ("tagsoup.jar", "Apache 2"),
      ("wrapper.jar", "MIT"),
      ("wsdl4j-1.5.1.jar", "CPL"),
      ("xercesImpl-2.8.1.jar", "Apache 2"),
      ("xercesImpl.jar", "Apache 2"),
      ("xml-apis-1.3.03.jar", "???"),
      ("xml-apis.jar", "???")]      

-- read Eclipse project info from a directory
readProject :: FilePath -> IO Project
readProject projectDir = do
  root <- readDocument $ projectDir </> ".project"
  classpathFileExists <- doesFileExist classpathFile

  let name = show . content . nameTag $ CElem root

  if classpathFileExists then do 
     classpath <- readDocument classpathFile
     return $ Project name (dependencies classpath) (libraries classpath)
    else return $ Project name [] []

  where
    classpathFile = projectDir </> ".classpath"

    nameTag = head . (keep /> tag "name" /> txt)

    -- scan external dependencies for referenced Java projects
    dependencies :: Element -> [String]
    dependencies =
      map tail . filter isExternal . map path . attributesForKind "src"

    -- scan external dependencies for referenced JAR libraries
    libraries :: Element -> [String]
    libraries = 
      filter isJar . map (takeFileName . path) . attributesForKind "lib"

    attributesForKind :: String -> Element -> [Attribute]
    attributesForKind kind = concatMap attributes . entries kind

    entries :: String -> Element -> [Content]
    entries kind = 
      (keep /> tag "classpathentry" `with` (attrEq "kind" kind)) . CElem

    isJar f = takeExtension f == ".jar"

    isExternal ('/' : cs) = True
    isExternal _          = False

    path :: Attribute -> String
    path ("path", value) = attr2str value
    path _               = ""

    attributes :: Content -> [Attribute]
    attributes (CElem (Elem _ attrs _)) = attrs
    attributes _                        = fail "Non-element selected"

    attrEq name value = attrval (name, AttValue [Left value])
    
-- read an XML document, return its root

readDocument :: FilePath -> IO Element
readDocument path = do
  xml <- readXml path

  case xml of
    Left message                -> fail message
    Right (Document _ _ root _) -> return root

  where
    readXml :: FilePath -> IO (Either String Document)
    readXml file = readFile file >>= return . xmlParse' file

test :: IO [Project]
test = mapM readProject 
  ["/Users/matt/Development/trunk/dfc.swt", 
   "/Users/matt/Development/trunk/livespaces/livespace.ui.media_viewer",
   "/Users/matt/Development/trunk/livespaces/livespace.eclipse_rcp",
   "/Users/matt/Development/trunk/livespaces/livespace.services",
   "/Users/matt/Development/trunk/livespaces/livespace.osgi",
   "/Users/matt/Development/avis/c"]

test2 :: Map String Project
test2 = 
  Map.fromList [(name p, p) | p <- ps]
  where
    ps = [Project "p1" ["p2", "p3"] [], Project "p2" ["p4"] []]

test_dot = test >>= return . dotGraph . reduceDependencies 

test_projects = [Project "a" ["b", "c", "d"] [], Project "b" ["c", "d"] ["lib1.jar"], 
                 Project "c" ["d"] [], Project "d" [] ["lib2.jar", "lib3.jar"]]
