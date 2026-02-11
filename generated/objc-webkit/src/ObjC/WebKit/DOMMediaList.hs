{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMMediaList@.
module ObjC.WebKit.DOMMediaList
  ( DOMMediaList
  , IsDOMMediaList(..)
  , item
  , deleteMedium
  , appendMedium
  , mediaText
  , setMediaText
  , length_
  , itemSelector
  , deleteMediumSelector
  , appendMediumSelector
  , mediaTextSelector
  , setMediaTextSelector
  , lengthSelector


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

-- | @- item:@
item :: IsDOMMediaList domMediaList => domMediaList -> CUInt -> IO (Id NSString)
item domMediaList  index =
  sendMsg domMediaList (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- deleteMedium:@
deleteMedium :: (IsDOMMediaList domMediaList, IsNSString oldMedium) => domMediaList -> oldMedium -> IO ()
deleteMedium domMediaList  oldMedium =
withObjCPtr oldMedium $ \raw_oldMedium ->
    sendMsg domMediaList (mkSelector "deleteMedium:") retVoid [argPtr (castPtr raw_oldMedium :: Ptr ())]

-- | @- appendMedium:@
appendMedium :: (IsDOMMediaList domMediaList, IsNSString newMedium) => domMediaList -> newMedium -> IO ()
appendMedium domMediaList  newMedium =
withObjCPtr newMedium $ \raw_newMedium ->
    sendMsg domMediaList (mkSelector "appendMedium:") retVoid [argPtr (castPtr raw_newMedium :: Ptr ())]

-- | @- mediaText@
mediaText :: IsDOMMediaList domMediaList => domMediaList -> IO (Id NSString)
mediaText domMediaList  =
  sendMsg domMediaList (mkSelector "mediaText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMediaText:@
setMediaText :: (IsDOMMediaList domMediaList, IsNSString value) => domMediaList -> value -> IO ()
setMediaText domMediaList  value =
withObjCPtr value $ \raw_value ->
    sendMsg domMediaList (mkSelector "setMediaText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- length@
length_ :: IsDOMMediaList domMediaList => domMediaList -> IO CUInt
length_ domMediaList  =
  sendMsg domMediaList (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @deleteMedium:@
deleteMediumSelector :: Selector
deleteMediumSelector = mkSelector "deleteMedium:"

-- | @Selector@ for @appendMedium:@
appendMediumSelector :: Selector
appendMediumSelector = mkSelector "appendMedium:"

-- | @Selector@ for @mediaText@
mediaTextSelector :: Selector
mediaTextSelector = mkSelector "mediaText"

-- | @Selector@ for @setMediaText:@
setMediaTextSelector :: Selector
setMediaTextSelector = mkSelector "setMediaText:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

