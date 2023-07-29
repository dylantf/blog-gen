module HsBlog.Directory (buildIndex, convertDirectory) where

import Control.Exception (SomeException (..), catch, displayException)
import Control.Monad (void, when)
import Data.List (partition)
import Data.Traversable (for)
import HsBlog.Convert (convert, convertStructure)
import HsBlog.Env (Env (..))
import HsBlog.Html qualified as Html
import HsBlog.Markup qualified as Markup
import System.Directory (
  copyFile,
  createDirectory,
  doesDirectoryExist,
  listDirectory,
  removeDirectoryRecursive,
 )
import System.Exit (exitFailure)
import System.FilePath (
  takeBaseName,
  takeExtension,
  takeFileName,
  (<.>),
  (</>),
 )
import System.IO (hPutStrLn, stderr)

buildIndex :: Env -> [(FilePath, Markup.Document)] -> Html.Html
buildIndex env files =
  let previews =
        map
          ( \(file, doc) ->
              case doc of
                Markup.Heading 1 heading : article ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                    <> foldMap convertStructure (take 3 article)
                    <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                _ ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ file))
          )
          files
   in Html.html_
        ( Html.title_ (eBlogName env)
            <> Html.stylesheet_ (eStylesheetPath env)
        )
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat previews
        )

{- | Copy files from one directory to another, converting .txt files to .html
  Errors are logged to stderr

May throw an exception on output directory creation.
-}
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done!"

data DirContents = DirContents
  { dcFilesToProcess :: [(FilePath, String)]
  , dcFilesToCopy :: [FilePath]
  }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let (txtFiles, otherFiles) = partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <- applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $
    DirContents
      { dcFilesToProcess = txtFilesAndContent
      , dcFilesToCopy = otherFiles
      }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList ioFn items = do
  for items $ \input -> do
    maybeResult <-
      catch
        (Right <$> ioFn input)
        ( \(SomeException ex) -> do
            pure $ Left (displayException ex)
        )
    pure (input, maybeResult)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures = do
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled" *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory path = do
  dirExists <- doesDirectoryExist path
  create <-
    if dirExists
      then do
        override <- confirm "Output directory already exists, override?"
        when override (removeDirectoryRecursive path)
        pure override
      else pure True
  when create (createDirectory path)
  pure create

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let txtOutputFiles = map toOutputMarkupFile txtFiles
      index = ("index.html", buildIndex txtOutputFiles)
   in map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (path, doc) = (path, convert path doc)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let writeFileContent (file, content) = writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

-- * Utils

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response, use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  when result action
