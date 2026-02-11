{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PKInk provides a description of how marks on a PKCanvas render and are created.
--
-- Generated bindings for @PKInk@.
module ObjC.PencilKit.PKInk
  ( PKInk
  , IsPKInk(..)
  , initWithInkType_color
  , inkType
  , color
  , requiredContentVersion
  , initWithInkType_colorSelector
  , inkTypeSelector
  , colorSelector
  , requiredContentVersionSelector

  -- * Enum types
  , PKContentVersion(PKContentVersion)
  , pattern PKContentVersion1
  , pattern PKContentVersion2
  , pattern PKContentVersion3
  , pattern PKContentVersion4
  , pattern PKContentVersionLatest

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

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithInkType:color:@
initWithInkType_color :: (IsPKInk pkInk, IsNSString type_, IsNSColor color) => pkInk -> type_ -> color -> IO (Id PKInk)
initWithInkType_color pkInk  type_ color =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr color $ \raw_color ->
      sendMsg pkInk (mkSelector "initWithInkType:color:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_color :: Ptr ())] >>= ownedObject . castPtr

-- | The type of ink, eg. pen, pencil...
--
-- ObjC selector: @- inkType@
inkType :: IsPKInk pkInk => pkInk -> IO (Id NSString)
inkType pkInk  =
  sendMsg pkInk (mkSelector "inkType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- color@
color :: IsPKInk pkInk => pkInk -> IO (Id NSColor)
color pkInk  =
  sendMsg pkInk (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The PencilKit version required to use this ink.
--
-- ObjC selector: @- requiredContentVersion@
requiredContentVersion :: IsPKInk pkInk => pkInk -> IO PKContentVersion
requiredContentVersion pkInk  =
  fmap (coerce :: CLong -> PKContentVersion) $ sendMsg pkInk (mkSelector "requiredContentVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInkType:color:@
initWithInkType_colorSelector :: Selector
initWithInkType_colorSelector = mkSelector "initWithInkType:color:"

-- | @Selector@ for @inkType@
inkTypeSelector :: Selector
inkTypeSelector = mkSelector "inkType"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @requiredContentVersion@
requiredContentVersionSelector :: Selector
requiredContentVersionSelector = mkSelector "requiredContentVersion"

