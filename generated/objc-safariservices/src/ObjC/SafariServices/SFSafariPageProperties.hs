{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariPageProperties@.
module ObjC.SafariServices.SFSafariPageProperties
  ( SFSafariPageProperties
  , IsSFSafariPageProperties(..)
  , url
  , title
  , usesPrivateBrowsing
  , active
  , urlSelector
  , titleSelector
  , usesPrivateBrowsingSelector
  , activeSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- url@
url :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO (Id NSURL)
url sfSafariPageProperties  =
  sendMsg sfSafariPageProperties (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO (Id NSString)
title sfSafariPageProperties  =
  sendMsg sfSafariPageProperties (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- usesPrivateBrowsing@
usesPrivateBrowsing :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO Bool
usesPrivateBrowsing sfSafariPageProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSafariPageProperties (mkSelector "usesPrivateBrowsing") retCULong []

-- | @- active@
active :: IsSFSafariPageProperties sfSafariPageProperties => sfSafariPageProperties -> IO Bool
active sfSafariPageProperties  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSafariPageProperties (mkSelector "active") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @usesPrivateBrowsing@
usesPrivateBrowsingSelector :: Selector
usesPrivateBrowsingSelector = mkSelector "usesPrivateBrowsing"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

