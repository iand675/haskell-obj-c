{-# LANGUAGE DataKinds #-}
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
  , fileURLSelector
  , initSelector
  , initWithFileURLSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKAsset ckAsset => ckAsset -> IO (Id CKAsset)
init_ ckAsset =
  sendOwnedMessage ckAsset initSelector

-- | @+ new@
new :: IO (Id CKAsset)
new  =
  do
    cls' <- getRequiredClass "CKAsset"
    sendOwnedClassMessage cls' newSelector

-- | Initialize an asset to be saved with the content at the given file URL
--
-- ObjC selector: @- initWithFileURL:@
initWithFileURL :: (IsCKAsset ckAsset, IsNSURL fileURL) => ckAsset -> fileURL -> IO (Id CKAsset)
initWithFileURL ckAsset fileURL =
  sendOwnedMessage ckAsset initWithFileURLSelector (toNSURL fileURL)

-- | Local file URL where fetched records are cached and saved records originate from.
--
-- ObjC selector: @- fileURL@
fileURL :: IsCKAsset ckAsset => ckAsset -> IO (Id NSURL)
fileURL ckAsset =
  sendMessage ckAsset fileURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKAsset)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithFileURL:@
initWithFileURLSelector :: Selector '[Id NSURL] (Id CKAsset)
initWithFileURLSelector = mkSelector "initWithFileURL:"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

