{-# LANGUAGE DataKinds #-}
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
  , allowsDirectoriesSelector
  , allowsMultipleSelectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether the file upload control supports multiple files.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsWKOpenPanelParameters wkOpenPanelParameters => wkOpenPanelParameters -> IO Bool
allowsMultipleSelection wkOpenPanelParameters =
  sendMessage wkOpenPanelParameters allowsMultipleSelectionSelector

-- | Whether the file upload control supports selecting directories.
--
-- ObjC selector: @- allowsDirectories@
allowsDirectories :: IsWKOpenPanelParameters wkOpenPanelParameters => wkOpenPanelParameters -> IO Bool
allowsDirectories wkOpenPanelParameters =
  sendMessage wkOpenPanelParameters allowsDirectoriesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @allowsDirectories@
allowsDirectoriesSelector :: Selector '[] Bool
allowsDirectoriesSelector = mkSelector "allowsDirectories"

