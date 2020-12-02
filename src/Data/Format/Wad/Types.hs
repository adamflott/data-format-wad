module Data.Format.Wad.Types
    (
      -- * Wad Type
    WadFileType(..)

    -- * On Disk Types
    ,  WadOD(..)
    , WadODHeader(..)
    , WadODEntry(..)

    -- * User Types
    , Wad(..)
    , Lump(..)
    , UDMFMap(..)

    -- * Type Construction
    , buildFileType
    , buildHeader

    -- * WAD Construction
    , newEmptyIWad
    , newEmptyPWad

    -- * Lump Manipulation
    , addUDMFMap

    -- * WAD Writing
    , writeWadToFile

    -- * Misc
    , buildWad
    , buildWadOD
    , buildLumpName
    )
where

import           Relude

-- base
import qualified Text.Show                      ( Show(..) )

-- Hackage
import           Data.ByteString.Builder
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC


data WadFileType = IWAD
                 | PWAD
                 deriving stock (Generic)

instance Show WadFileType where
    show = \case
        IWAD -> "IWAD"
        PWAD -> "PWAD"



data WadOD = WadOD {
    _wadODHeader :: WadODHeader
    , _wadODData :: Builder
    , _wadODDirectory :: [WadODEntry]
    } deriving stock (Generic)

instance Show WadOD where
    show (WadOD hdr _ dir) = show hdr <> show dir

data WadODHeader = WadODHeader {
    _wadODHdrFileType :: WadFileType -- ^
    , _wadODHdrLumpCount :: Int32 -- ^
    , _wadODHdrDirectoryOffset :: Int32 -- ^
    }
    deriving stock (Generic, Show)


data WadODEntry = WadODEntry {
    _wadODEntryOffset :: Int32 -- ^
    , _wadODEntrySize :: Int32 -- ^
    , _wadODEntryName :: LumpName -- ^
    }
    deriving stock (Generic, Show)

newtype LumpName = LumpName ByteString
                 deriving stock (Generic, Show)


--

data Wad = Wad {
    _wadFileType :: WadFileType
    , _wadEntries :: [Lump]
    }
    deriving stock (Generic)


instance Show Wad where
    show (Wad ft entries) = "WAD type: " <> show ft <> "\n" <> "WAD number of entries: " <> show (length entries) <> "\n" <> show entries

data Lump
    = LumpTextMap UDMFMap
    deriving stock (Generic, Show)

data UDMFMap = UDMFMap LumpName ByteString
             deriving stock (Generic, Show)

--

-- unpadded string
buildFileType :: WadFileType -> Builder
buildFileType = \case
    IWAD -> string7 "IWAD"
    PWAD -> string7 "PWAD"

buildHeader :: WadODHeader -> Builder
buildHeader (WadODHeader ty lump_count dir_offset) = buildFileType ty <> int32LE lump_count <> int32LE dir_offset

buildLumpName :: LumpName -> Either Text Builder
buildLumpName (LumpName ln) = if BS.length ln > maxStringLength
    then Left ("Lump name large than " <> show (maxStringLength :: Integer) <> " characters")
    else if not (BSC.any checkChars ln)
        then Left "Lump name contains characters outside the set: A-Z (uppercase), 0-9, '[', ']', '-', '_'"
        else Right (padRightString ln)

  where
    checkChars :: Char -> Bool
    checkChars c | c `elem` ['[', ']', '-', '_'] = True
                 | c `elem` ['A' .. 'Z']         = True
                 | otherwise                     = False

---


padRightString :: ByteString -> Builder
padRightString s = byteString s <> byteString (BS.replicate (maxStringLength - BS.length s) 0)

maxStringLength :: Num a => a
maxStringLength = 8

--


newEmptyIWad :: Wad
newEmptyIWad = Wad IWAD []

newEmptyPWad :: Wad
newEmptyPWad = Wad PWAD []

--

addUDMFMap :: Text -> Text -> Wad -> Wad
addUDMFMap map_name map_data wad@(Wad _ es) =
    wad { _wadEntries = es <> [LumpTextMap (UDMFMap (LumpName (encodeUtf8 map_name)) (encodeUtf8 map_data))] }


--

writeWadToFile :: MonadIO m => Text -> Wad -> m (Maybe Text)
writeWadToFile filename wad = do
    let od       = buildWadOD wad
        contents = buildWad od

    writeFileLBS (toString filename) (toLazyByteString contents)
    pure Nothing


--

buildWadOD :: Wad -> WadOD
buildWadOD (Wad ty es) =
    let (dir_offset, od_data, dir_entries) = mkDirEntries
    in  WadOD { _wadODHeader = WadODHeader ty (fromIntegral (length dir_entries)) dir_offset, _wadODData = od_data, _wadODDirectory = dir_entries }
  where
    headerOffset = 12 -- 4 bytes for ASCII wad type + 4 bytes for number of lumps + 4 bytes for offset to find directory entires

    mkDirEntries = let ((offset, od_data), entries) = mapAccumL f (headerOffset, mempty) es in (offset, od_data, concat entries)

    f :: (Int32, Builder) -> Lump -> ((Int32, Builder), [WadODEntry])
    f (cur_dir_offset, b) lump =
      case lump of
        LumpTextMap (UDMFMap ln map_data) ->
         let map_data_size = fromIntegral (BS.length map_data)
         in
            ( (cur_dir_offset + map_data_size , b <> byteString map_data)
            , [ WadODEntry cur_dir_offset 0 ln
              , WadODEntry cur_dir_offset map_data_size lumpBeginTextMap
              , WadODEntry (cur_dir_offset + map_data_size) 0 (LumpName "BEHAVIOR")
              , WadODEntry (cur_dir_offset + map_data_size) 0 lumpEndTextMap
              ]
            )

    lumpBeginTextMap = LumpName "TEXTMAP"
    lumpEndTextMap   = LumpName "ENDMAP"


buildWadEntries :: [WadODEntry] -> Builder
buildWadEntries es = mconcat (fmap buildWadEntry es)

buildWadEntry :: WadODEntry -> Builder
buildWadEntry (WadODEntry offset size (LumpName name)) = int32LE offset <> int32LE size <> padRightString name


buildWad :: WadOD -> Builder
buildWad (WadOD hdr od_data dir_entries) = buildHeader hdr <> od_data <> buildWadEntries dir_entries

