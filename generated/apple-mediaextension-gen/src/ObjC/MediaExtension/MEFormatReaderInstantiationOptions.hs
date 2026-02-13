{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEFormatReaderInstantiationOptions
--
-- A class that encapsulates options to be passed to MEFormatReaderExtension
--
-- The class MEFormatReaderInstantiationOptions is mutable, with options set through instance properties.
--
-- Generated bindings for @MEFormatReaderInstantiationOptions@.
module ObjC.MediaExtension.MEFormatReaderInstantiationOptions
  ( MEFormatReaderInstantiationOptions
  , IsMEFormatReaderInstantiationOptions(..)
  , allowIncrementalFragmentParsing
  , allowIncrementalFragmentParsingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | allowIncrementalFragmentParsing
--
-- Enables support for parsing additional fragments
--
-- If YES, requests that the MEFormatReader be configured to support calls to parseAdditionalFragments. By default the MEFormatReader does not support calls to parseAdditionalFragments.
--
-- ObjC selector: @- allowIncrementalFragmentParsing@
allowIncrementalFragmentParsing :: IsMEFormatReaderInstantiationOptions meFormatReaderInstantiationOptions => meFormatReaderInstantiationOptions -> IO Bool
allowIncrementalFragmentParsing meFormatReaderInstantiationOptions =
  sendMessage meFormatReaderInstantiationOptions allowIncrementalFragmentParsingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allowIncrementalFragmentParsing@
allowIncrementalFragmentParsingSelector :: Selector '[] Bool
allowIncrementalFragmentParsingSelector = mkSelector "allowIncrementalFragmentParsing"

