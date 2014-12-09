{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ExistentialQuantification, RecordWildCards, PackageImports, NoImplicitPrelude #-}
module System.Mock.IO.Internal (
    IO, Handle, IOMode, SeekMode, FilePath, HandlePosn, User, Server, Direction (In, Out),
    RealWorld (RealWorld, handles, files, workDir, isPermitted, nextHandle),
    newWorld, emptyWorld, mkUser, setUser, mkServer, addServer, removeServer, listServers, runIO, evalIO, stdin, stdout, stderr,
    withFile, openFile, hClose, readFile, writeFile, appendFile, doesFileExist,
    connectTo,
    hFileSize, hSetFileSize, hIsEOF, isEOF, hGetBuffering, hSetBuffering, hFlush,
    hGetPosn, hSetPosn, hSeek, hTell,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable, hIsTerminalDevice, hShow,
    hGetChar, hGetLine, hLookAhead, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, readIO, readLn, interact,
    dumpHandle, usrStdin, usrStdout, usrStderr, reverseHandle, getOpenHandles
  ) where

import "base" Prelude hiding (FilePath, IO, getLine, getChar, readIO, readLn, putStr, putStrLn, putChar, 
                              readFile, writeFile, appendFile, getContents, interact)
import qualified "network" Network as N

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.List.Split
import Data.List (intercalate, genericIndex)

type FilePath = String
type PortID = N.PortID
type HostName = N.HostName

interpretPath :: FilePath -> FilePath -> FilePath
interpretPath p1 p2 = normalizePath $ if head p2 == '/' then p2 else p1 ++ "/" ++ p2

isValidPath :: FilePath -> Bool
isValidPath "" = False
isValidPath p = all (== '/') p || last p /= '/'

normalizePath :: FilePath -> FilePath
normalizePath p
    | abs p = fromPartsAbs (normAbs parts [])
    | otherwise = fromPartsRel (normRel parts [])
  where
    abs ('/':_) = True
    abs _ = False
    parts = filter (not . null) $ filter (/= ".") $ splitOn "/" $ p
    fromPartsAbs p = "/" ++ intercalate "/" p
    fromPartsRel p = if null p then "." else intercalate "/" p
    normAbs [] as = reverse as
    normAbs (".." : ps) [] = normAbs ps []
    normAbs (".." : ps) (a : as) = normAbs ps as
    normAbs (p : ps) as = normAbs ps (p : as)
    normRel [] as = reverse as
    normRel (".." : ps) [] = normRel ps [".."]
    normRel (".." : ps) (a : as)
        | a /= ".." = normRel ps as
    normRel (p : ps) as = normRel ps (p : as)

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode deriving (Show, Eq, Ord, Read, Enum)
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd deriving (Show, Eq, Ord, Read, Enum)

allowsReading :: IOMode -> Bool
allowsReading m = m == ReadMode || m == ReadWriteMode

allowsWriting :: IOMode -> Bool
allowsWriting m = m /= ReadMode


data SpecialFile = StdIn | StdOut | StdErr deriving (Eq, Ord)

instance Show SpecialFile where
  show StdIn = "<stdin>"
  show StdOut = "<stdout>"
  show StdErr = "<stderr>"

data Direction = In | Out deriving (Eq, Ord, Show)
data File = RegularFile FilePath | NetworkSocket Integer String Direction | SpecialFile SpecialFile deriving (Eq, Ord)

instance Show File where
  show (RegularFile p) = p
  show (SpecialFile t) = show t
  show (NetworkSocket _ s _) = s



data Handle = Handle {_hId :: Integer, _hName :: String, _hInFile :: File, _hOutFile :: File,
                      _hReverseHandle :: Maybe Handle}

instance Eq Handle where
  h1 == h2 = _hId h1 == _hId h2

instance Ord Handle where
  compare h1 h2 = compare (_hId h1) (_hId h2)

instance Show Handle where
  show = show . _hName

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int) deriving (Eq, Ord, Read, Show)

data HandleData = HandleData {
  _hGetMode :: IOMode,
  _hIsOpen :: Bool,
  _hIsSeekable :: Bool,
  _hBufferMode :: BufferMode,
  _hBufPos :: Integer
}

_hIsFile :: Handle -> Bool
_hIsFile (Handle _ _(RegularFile _) _ _) = True
_hIsFile _ = False

seekError :: String -> Handle -> a
seekError s h = error (show h ++ ": " ++ s ++ ": illegal operation (handle is not seekable)")

invalidArgError :: String -> Handle -> a
invalidArgError s h = error (show h ++ ": " ++ s ++ ": illegal operation (Invalid argument)")

data User = forall u. User u (u -> IO u)
data Server = forall s. Server s (Handle -> s -> IO s)

mkUser :: u -> (u -> IO u) -> User
mkUser = User

mkServer :: u -> (Handle -> u -> IO u) -> Server
mkServer = Server

data RealWorld = forall u. RealWorld {
  workDir :: FilePath,
  files :: Map File Text,
  isPermitted :: FilePath -> IOMode -> Bool,
  handles :: Map Handle HandleData,
  nextHandle :: Integer,
  user :: User,
  servers :: Map String Server
}

setUser :: User -> RealWorld -> RealWorld
setUser usr w = w {user = usr}

runUser :: IO ()
runUser =
  do User userState userAction <- fmap user getWorld
     userState' <- userAction userState
     w <- getWorld
     putWorld (setUser (User userState' userAction) w)

getServer :: String -> IO (Maybe Server)
getServer s = fmap (M.lookup s . servers) getWorld

addServer :: HostName -> PortID -> Server -> RealWorld -> RealWorld
addServer host port server w = w {servers = M.insert (showConnectionInfo host port) server (servers w)}

removeServer :: HostName -> PortID -> RealWorld -> RealWorld
removeServer host port w = w {servers = M.delete (showConnectionInfo host port) (servers w)}

putServer :: String -> Server -> IO ()
putServer s server = getWorld >>= (\w -> putWorld (w {servers = M.insert s server (servers w)}))

listServers :: IO [String]
listServers = fmap (map fst . M.toList . servers) getWorld

runServer :: String -> Handle -> IO ()
runServer name h = 
  do server <- getServer name
     case server of
        Nothing -> return ()
        Just (Server serverState serverAction) ->
          do serverState' <- serverAction h serverState
             putServer name (Server serverState' serverAction) 

newtype IO a = IO { getIO :: State RealWorld a } deriving (Functor, Applicative, Monad)

getWorld :: IO RealWorld
getWorld = IO get

putWorld :: RealWorld -> IO ()
putWorld = IO . put

evalIO :: IO a -> RealWorld -> a
evalIO (IO s) = fst . runState s

runIO :: IO a -> RealWorld -> (a, RealWorld)
runIO (IO s) = runState s

mkSpecialHandle :: Integer -> SpecialFile -> Maybe Handle -> Handle
mkSpecialHandle id t h = Handle id (show t) (SpecialFile t) (SpecialFile t) h

specialHandles@[stdin, stdout, stderr, usrStdin, usrStdout, usrStderr] = 
    zipWith3 mkSpecialHandle [-1,-2..] [StdIn, StdOut, StdErr, StdIn, StdOut, StdErr] 
             [Just usrStdin, Just usrStdout, Just usrStderr, Nothing, Nothing, Nothing]

reverseSpecialHandle :: Integer -> Handle
reverseSpecialHandle i = cycle specialHandles `genericIndex` (2 - i)

newWorld :: FilePath -> [(FilePath, Text)] -> (FilePath -> IOMode -> Bool) -> RealWorld
newWorld workDir files permitted
  = RealWorld workDir
              (M.fromList ([(SpecialFile t, "") | t <- [StdIn, StdOut, StdErr]] ++
                           [(RegularFile path, content) | (path, content) <- files]))
              permitted
              (M.fromList [(stdin, HandleData ReadMode True False LineBuffering 0),
                           (stdout, HandleData AppendMode True False LineBuffering 0),
                           (stderr, HandleData AppendMode True False LineBuffering 0),
                           (usrStdin, HandleData WriteMode True False LineBuffering 0),
                           (usrStdout, HandleData ReadMode True False LineBuffering 0),
                           (usrStderr, HandleData ReadMode True False LineBuffering 0)])
              0 (mkUser () (const (return ()))) (M.empty)

emptyWorld :: RealWorld
emptyWorld = newWorld "/" [] (\_ _ -> True)

getHData :: String -> Handle -> IO HandleData
getHData s h = do d <- fmap (M.lookup h . handles) getWorld
                  case d of
                    Nothing -> error (show h ++ ": " ++ s ++ ": invalid handle")
                    Just d  -> return d

putHData :: Handle -> HandleData -> IO ()
putHData h d = do w <- getWorld
                  putWorld (w { handles = M.insert h d (handles w) })

hShow :: Handle -> IO String
hShow h =
  do d <- getHData "hShow" h
     let t = case _hGetMode d of
               ReadMode -> "readable"
               WriteMode -> "writable"
               AppendMode -> "writable (append)"
               ReadWriteMode -> "read-writable"
     let s = if _hIsOpen d then
               "{loc=" ++ show h ++ ",type=" ++ t ++ ",buffering=none}"
             else
               "{closed}"
     return s

hIsSeekable :: Handle -> IO Bool
hIsSeekable h = fmap _hIsSeekable (getHData "hIsSeekable" h)

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice (Handle _ _ (SpecialFile t) _ _) = return (t == StdIn || t == StdOut || t == StdErr)
hIsTerminalDevice _ = return False

getFileContents :: String -> File -> IO Text
getFileContents s f = 
  do w <- getWorld
     case M.lookup f (files w) of
       Nothing -> fail (show f ++ ": " ++ s ++ ": does not exist (No such file or directory)")
       Just t  -> return t

putFileContents :: File -> Text -> IO ()
putFileContents f t =
  do w <- getWorld
     putWorld (w {files = M.insert f t (files w)})

fileSize :: File -> IO Integer
fileSize f = fmap (fromIntegral . T.length) (getFileContents "fileSize" f)

                  
type HandlePosition = Integer
data HandlePosn = HandlePosn Handle HandlePosition

instance Eq HandlePosn where
    (HandlePosn h1 p1) == (HandlePosn h2 p2) = p1==p2 && h1==h2

instance Show HandlePosn where
   showsPrec p (HandlePosn h pos) = 
        showsPrec p h . showString " at position " . shows pos

hEnsureOpen' :: String -> Handle -> HandleData -> IO ()
hEnsureOpen' s h d = 
  if _hIsOpen d then return () else fail (show h ++ ": " ++ s ++ ": illegal operation (handle is closed)")

hEnsureOpen :: String -> Handle -> IO ()
hEnsureOpen s h = getHData s h >>= hEnsureOpen' s h

hIsOpen :: Handle -> IO Bool
hIsOpen h = fmap _hIsOpen (getHData "hIsOpen" h)

hIsClosed :: Handle -> IO Bool
hIsClosed h = fmap _hIsOpen (getHData "hIsClosed" h)

fileExists :: RealWorld -> File -> Bool
fileExists w f = (case f of {RegularFile p -> isValidPath p; _ -> True}) && M.member f (files w)

doesFileExist :: FilePath -> IO Bool
doesFileExist p = fileExists <$> getWorld <*> mkFile p

mkPath :: FilePath -> IO FilePath
mkPath path = fmap (\w -> interpretPath (workDir w) path) getWorld

mkFile :: FilePath -> IO File
mkFile = fmap RegularFile . mkPath

mkHandle :: String -> (Integer -> File) -> (Integer -> File) -> IOMode -> Bool -> Integer -> IO Handle
mkHandle name inFile outFile mode seekable pos =
  do w <- getWorld
     let id = nextHandle w
     let h = Handle id name (inFile id) (outFile id) Nothing
     let d = HandleData mode True seekable LineBuffering pos
     putWorld (w {nextHandle = id + 1, handles = M.insert h d (handles w)})
     return h

reverseHandle :: Handle -> Handle
reverseHandle h = case _hReverseHandle h of 
                    Nothing -> error (show h ++ ": handle does not have a reverse handle")
                    Just h' -> h'

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p mode) then
       fail (show f ++ ": " ++ "openFile: permission denied (Permission denied)")
     else do
       let ex = fileExists w f
       when (not ex) $
         if mode == ReadMode then 
           fail (show f ++ ": openFile: does not exist (No such file or directory)")
         else
           writeFile p ""
       pos <- if mode == AppendMode then fmap (fromIntegral . T.length) (getFileContents "openFile" f) else return 0
       mkHandle p (const f) (const f) mode True pos

showConnectionInfo :: HostName -> PortID -> String
showConnectionInfo h (N.Service s) = s ++ "://" ++ h
showConnectionInfo h (N.PortNumber n) = h ++ ":" ++ show n
showConnectionInfo h (N.UnixSocket s) = s

connectTo :: HostName -> PortID -> IO Handle
connectTo host port =
  do let name = showConnectionInfo host port
     w <- getWorld
     server <- getServer name
     case server of
       Nothing -> fail "getAddrInfo: does not exist (No address associated with hostname)"
       Just _  -> 
         do h <- mkHandle name (\id -> NetworkSocket id name In) (\id -> NetworkSocket id name Out) ReadWriteMode False 0
            h' <- mkHandle (name ++ " (server)") (const (_hOutFile h)) (const (_hInFile h)) ReadWriteMode False 0
            putFileContents (_hInFile h) ""
            putFileContents (_hOutFile h) ""
            return (h {_hReverseHandle = Just h'})

hClose :: Handle -> IO ()
hClose h = 
  do d <- getHData "hClose" h
     putHData h (d {_hIsOpen = False})
     maybe (return ()) hClose (_hReverseHandle h)

getOpenHandles :: IO [Handle]
getOpenHandles = 
  do w <- getWorld
     return [h | (h, d) <- M.toList (handles w), _hIsOpen d]

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode f =
  do h <- openFile path mode
     r <- f h
     hClose h
     return r

hTell :: Handle -> IO Integer
hTell h =
  do d <- getHData "hTell" h
     hEnsureOpen' "hTell" h d
     return (if _hIsSeekable d then fromIntegral (_hBufPos d) else seekError "hTell" h)

hFileSize :: Handle -> IO Integer
hFileSize h
  | _hIsFile h = 
      do d <- getHData "hFileSize" h
         hEnsureOpen' "hFileSize" h d
         t <- getFileContents "hFileSize" (_hInFile h)
         return (fromIntegral (T.length t))
  | otherwise = fail (show h ++ ": hFileSize: inappropriate type (not a regular file)")

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering h = fmap _hBufferMode (getHData "hGetBuffering" h)

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h m = getHData "hSetBuffering" h >>= (\d -> putHData h (d { _hBufferMode = m}))

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = HandlePosn h <$> hTell h

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn h p) = hSeek h AbsoluteSeek p

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek h mode pos =
  do d <- getHData "hSeek" h
     hEnsureOpen' "hSeek" h d
     if not (_hIsSeekable d) then seekError "hSeek" h else return ()
     t <- getFileContents "hSeek" (_hInFile h)
     let size = fromIntegral (T.length t)
     let pos' = case mode of
                  AbsoluteSeek -> fromIntegral pos
                  RelativeSeek -> _hBufPos d + fromIntegral pos
                  SeekFromEnd  -> size - fromIntegral pos
     let d' = if pos' < 0 then invalidArgError "hSeek" h else d {_hBufPos = pos'}
     putHData h d'

hIsEOF :: Handle -> IO Bool
hIsEOF h =
  do d <- getHData "hIsEOF" h
     hEnsureOpen' "hIsEOF" h d
     t <- getFileContents "hIsEOF" (_hInFile h)
     return (_hBufPos d >= fromIntegral (T.length t))

isEOF :: IO Bool
isEOF = hIsEOF stdin

hIsReadable :: Handle -> IO Bool
hIsReadable h = (allowsReading . _hGetMode) <$> getHData "hIsReadable" h

hIsWritable :: Handle -> IO Bool
hIsWritable h = (allowsWriting . _hGetMode) <$> getHData "hIsWritable" h

dumpHandle :: Handle -> Direction -> IO Text
dumpHandle h d = 
  do hEnsureOpen "dumpHandle" h
     getFileContents "dumpHandle" (if d == In then _hInFile h else _hOutFile h)

_hSetFileSize :: String -> Handle -> HandleData -> Integer -> IO ()
_hSetFileSize s h d size =
  do hEnsureOpen' s h d
     if allowsWriting (_hGetMode d) then return ()
        else fail (show h ++ ": " ++ s ++ ": illegal operation (handle is not open for writing)")
     t <- getFileContents s (_hOutFile h)
     let diff = fromIntegral size - fromIntegral (T.length t)
     case compare diff 0 of
       EQ -> return ()
       GT -> putFileContents (_hOutFile h) (T.append t (T.replicate diff (T.singleton '\0')))
       LT -> putFileContents (_hOutFile h) (T.take (fromIntegral size) t)

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize h size = 
  do d <- getHData "hSetFileSize" h
     _hSetFileSize "hSetFileSize" h d size

hPrepareWrite :: String -> Handle -> HandleData -> IO ()
hPrepareWrite s h d = _hSetFileSize s h d (_hBufPos d)

hPutText :: Handle -> Text -> IO ()
hPutText h s =
  do d <- getHData "hPutText" h
     hPrepareWrite "hPutText" h d
     let l = T.length s
     t <- getFileContents "hPutText" (_hOutFile h)
     let t' = case T.splitAt (fromIntegral (_hBufPos d)) t of
                (t1, t2) -> T.append t1 (T.append s (T.drop l t2))
     putHData h (d {_hBufPos = _hBufPos d + fromIntegral l})
     putFileContents (_hOutFile h) t'

hPutStr :: Handle -> String -> IO ()
hPutStr h s = hPutText h (T.pack s)

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutText h (T.pack (s ++ "\n"))

hPutChar :: Handle -> Char -> IO ()
hPutChar h c = hPutText h (T.singleton c)

hPrint :: Show a => Handle -> a -> IO ()
hPrint h x = hPutStrLn h (show x)

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

putChar :: Char -> IO ()
putChar = hPutChar stdout


hEnsureReadable :: String -> Handle -> HandleData -> IO ()
hEnsureReadable s h d
  | allowsReading (_hGetMode d) = return ()
  | otherwise = fail (show h ++ ": " ++ s ++ ": illegal operation (handle is not open for reading)")

_hGetText :: Handle -> Integer -> IO Text
_hGetText h s =
  do d <- getHData "hGetText" h
     hEnsureOpen' "hGetText" h d
     hEnsureReadable "hGetText" h d
     t <- getFileContents "hGetText" (_hInFile h)
     if _hBufPos d + fromIntegral s > fromIntegral (T.length t) then
       fail (show h ++ ": hGetText: end of file")
     else do
       let t' = T.take (fromIntegral s) (T.drop (fromIntegral (_hBufPos d)) t)
       putHData h (d {_hBufPos = _hBufPos d + fromIntegral s})
       return t'

_hLookAhead :: Handle -> IO Char
_hLookAhead h =
  do d <- getHData "hLookAhead" h
     hEnsureOpen' "hLookAhead" h d
     hEnsureReadable "hLookAhead" h d
     t <- getFileContents "hLookAhead" (_hInFile h)
     if _hBufPos d >= fromIntegral (T.length t) then
       fail (show h ++ ": hLookAhead: end of file")
     else
       return (T.index t (fromIntegral (_hBufPos d)))

hGetChar :: Handle -> IO Char
hGetChar h = fmap T.head (hGetText h 1)

_hGetContentText :: Handle -> IO Text
_hGetContentText h = 
  do d <- getHData "hGetContexts" h
     hEnsureOpen' "hGetContents" h d
     hEnsureReadable "hGetContents" h d
     t <- getFileContents "hGetContents" (_hInFile h)
     let t' = T.drop (fromIntegral (_hBufPos d)) t
     putHData h (d {_hBufPos = _hBufPos d + fromIntegral (T.length t')})
     return t'

_hGetLineText :: Handle -> IO Text
_hGetLineText h =
  do d <- getHData "hGetLine" h
     hEnsureOpen' "hGetLine" h d
     hEnsureReadable "hGetLine" h d
     t <- getFileContents "hGetLine" (_hInFile h)
     if _hBufPos d >= fromIntegral (T.length t) then
       fail (show h ++ ": hGetLine: end of file")
     else do
       let (t1, t2) = T.span (/= '\n') (T.drop (fromIntegral (_hBufPos d)) t)
       let s = fromIntegral (T.length t1) + (if T.isPrefixOf "\n" t2 then 1 else 0)
       putHData h (d {_hBufPos = _hBufPos d + s})
       return t1

hGetLine :: Handle -> IO String
hGetLine h = fmap T.unpack (hGetLineText h)

readFileText :: FilePath -> IO Text
readFileText path =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p ReadMode) then
       fail (path ++ ": " ++ "openFile: permission denied (Permission denied)")
     else
       mkFile path >>= getFileContents "openFile"

readFile :: FilePath -> IO String
readFile = fmap T.unpack . readFileText

writeFileText :: FilePath -> Text -> IO ()
writeFileText path t =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p WriteMode) then
       fail (path ++ ": " ++ "openFile: permission denied (Permission denied)")
     else do
       f <- mkFile path
       putFileContents f t

writeFile :: FilePath -> String -> IO()
writeFile path t = writeFileText path (T.pack t)

appendFileText :: FilePath -> Text -> IO ()
appendFileText path t =
  do w <- getWorld
     p <- mkPath path
     f <- mkFile path
     if not (isPermitted w p AppendMode) then
       fail (path ++ ": " ++ "openFile: permission denied (Permission denied)")
     else do
       f <- mkFile path
       w <- getWorld
       case M.lookup f (files w) of
         Nothing -> putFileContents f t
         Just t' -> putFileContents f (T.append t' t)

appendFile :: FilePath -> String -> IO()
appendFile path t = appendFileText path (T.pack t)

readIO :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ;
                              ("","") <- lex t ;
                              return x }) of
                        [x]    -> return x
                        []     -> fail "IO.readIO: no parse"
                        _      -> fail "IO.readIO: ambiguous parse"

_hIsNetworkHandle :: Handle -> Bool
_hIsNetworkHandle h = case _hInFile h of {NetworkSocket _ _ In -> True; _ -> False}

_hCanBlock :: Handle -> Bool
_hCanBlock h = h == stdin || _hIsNetworkHandle h

wrapBlockingOp :: String -> (Handle -> IO a) -> Handle -> IO a
wrapBlockingOp s op h
  | h == stdin =
      do hEnsureOpen s h
         eof <- isEOF
         if not eof then op h else do
           runUser
           eof <- isEOF
           if not eof then op h else fail (show stdin ++ ": " ++ s ++ ": user input expected, but user does not respond)")
  | _hIsNetworkHandle h =
      do hEnsureOpen s h
         eof <- hIsEOF h
         let host = case _hInFile h of NetworkSocket _ host _ -> host
         if not eof then op h else do
           runServer host (reverseHandle h)
           eof <- hIsEOF h
           if not eof then op h else fail (show h ++ ": " ++ s ++ ": response from server " ++ host ++ " expected, but server does not respond)")
  | otherwise = op h


getText :: Integer -> IO Text
getText = hGetText stdin

getChar :: IO Char
getChar = hGetChar stdin

getLineText :: IO Text
getLineText = hGetLineText stdin

getLine :: IO String
getLine = hGetLine stdin

lookAhead :: IO Char
lookAhead = hLookAhead stdin

hGetText :: Handle -> Integer -> IO Text
hGetText h s = wrapBlockingOp "hGetText" (\h -> _hGetText h s) h

hLookAhead :: Handle -> IO Char
hLookAhead = wrapBlockingOp "hLookAhead" _hLookAhead

hGetLineText :: Handle -> IO Text
hGetLineText = wrapBlockingOp "hGetLine" _hGetLineText


readLn :: Read a => IO a
readLn = getLine >>= readIO

getContentText :: IO Text
getContentText =
  do t <- _hGetContentText stdin
     runUser
     eof <- isEOF
     t' <- if eof then return t else fmap (T.append t) getContentText
     hClose stdin
     return t'

hGetContentText :: Handle -> IO Text
hGetContentText (Handle _ _ (SpecialFile StdIn) _ _) = getContentText
hGetContentText h = do {t <- _hGetContentText h; hClose h; return t}

hGetContents :: Handle -> IO String
hGetContents h = fmap T.unpack (hGetContentText h)

getContents :: IO String
getContents = fmap T.unpack getContentText

interact :: (String -> String) -> IO ()
interact f =
  do runUser
     eof <- isEOF
     if eof then return () else do
       s <- getLine
       putStrLn (f (s ++ "\n"))
       interact f

hFlush :: Handle -> IO ()
hFlush _ = return ()

