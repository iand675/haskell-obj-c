{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WKOpenPanelParameters contains parameters that a file upload control has specified.
--
-- Generated bindings for @WKOpenPanelParameters@.
module ObjC.WebKit.WKOpenPanelParameters
  ( WKOpenPanelParameters
  , IsWKOpenPanelParameters(..)
  , allowsMultipleSelection
  , allowsDirectories
  , allowsMultipleSelectionSelector
  , allowsDirectoriesSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the file upload control supports multiple files.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsWKOpenPanelParameters wkOpenPanelParameters => wkOpenPanelParameters -> IO Bool
allowsMultipleSelection wkOpenPanelParameters  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkOpenPanelParameters (mkSelector "allowsMultipleSelection") retCULong []

-- | Whether the file upload control supports selecting directories.
--
-- ObjC selector: @- allowsDirectories@
allowsDirectories :: IsWKOpenPanelParameters wkOpenPanelParameters => wkOpenPanelParameters -> IO Bool
allowsDirectories wkOpenPanelParameters  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkOpenPanelParameters (mkSelector "allowsDirectories") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @allowsDirectories@
allowsDirectoriesSelector :: Selector
allowsDirectoriesSelector = mkSelector "allowsDirectories"

