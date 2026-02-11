{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKAsset@.
module ObjC.CloudKit.CKAsset
  ( CKAsset
  , IsCKAsset(..)
  , init_
  , new
  , initWithFileURL
  , fileURL
  , initSelector
  , newSelector
  , initWithFileURLSelector
  , fileURLSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKAsset ckAsset => ckAsset -> IO (Id CKAsset)
init_ ckAsset  =
  sendMsg ckAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKAsset)
new  =
  do
    cls' <- getRequiredClass "CKAsset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize an asset to be saved with the content at the given file URL
--
-- ObjC selector: @- initWithFileURL:@
initWithFileURL :: (IsCKAsset ckAsset, IsNSURL fileURL) => ckAsset -> fileURL -> IO (Id CKAsset)
initWithFileURL ckAsset  fileURL =
withObjCPtr fileURL $ \raw_fileURL ->
    sendMsg ckAsset (mkSelector "initWithFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= ownedObject . castPtr

-- | Local file URL where fetched records are cached and saved records originate from.
--
-- ObjC selector: @- fileURL@
fileURL :: IsCKAsset ckAsset => ckAsset -> IO (Id NSURL)
fileURL ckAsset  =
  sendMsg ckAsset (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithFileURL:@
initWithFileURLSelector :: Selector
initWithFileURLSelector = mkSelector "initWithFileURL:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

