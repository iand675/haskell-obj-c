{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Transition animation subclass. *
--
-- Generated bindings for @CATransition@.
module ObjC.QuartzCore.CATransition
  ( CATransition
  , IsCATransition(..)
  , type_
  , setType
  , subtype
  , setSubtype
  , startProgress
  , setStartProgress
  , endProgress
  , setEndProgress
  , filter_
  , setFilter
  , typeSelector
  , setTypeSelector
  , subtypeSelector
  , setSubtypeSelector
  , startProgressSelector
  , setStartProgressSelector
  , endProgressSelector
  , setEndProgressSelector
  , filterSelector
  , setFilterSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsCATransition caTransition => caTransition -> IO (Id NSString)
type_ caTransition  =
  sendMsg caTransition (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsCATransition caTransition, IsNSString value) => caTransition -> value -> IO ()
setType caTransition  value =
withObjCPtr value $ \raw_value ->
    sendMsg caTransition (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtype@
subtype :: IsCATransition caTransition => caTransition -> IO (Id NSString)
subtype caTransition  =
  sendMsg caTransition (mkSelector "subtype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtype:@
setSubtype :: (IsCATransition caTransition, IsNSString value) => caTransition -> value -> IO ()
setSubtype caTransition  value =
withObjCPtr value $ \raw_value ->
    sendMsg caTransition (mkSelector "setSubtype:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startProgress@
startProgress :: IsCATransition caTransition => caTransition -> IO CFloat
startProgress caTransition  =
  sendMsg caTransition (mkSelector "startProgress") retCFloat []

-- | @- setStartProgress:@
setStartProgress :: IsCATransition caTransition => caTransition -> CFloat -> IO ()
setStartProgress caTransition  value =
  sendMsg caTransition (mkSelector "setStartProgress:") retVoid [argCFloat (fromIntegral value)]

-- | @- endProgress@
endProgress :: IsCATransition caTransition => caTransition -> IO CFloat
endProgress caTransition  =
  sendMsg caTransition (mkSelector "endProgress") retCFloat []

-- | @- setEndProgress:@
setEndProgress :: IsCATransition caTransition => caTransition -> CFloat -> IO ()
setEndProgress caTransition  value =
  sendMsg caTransition (mkSelector "setEndProgress:") retVoid [argCFloat (fromIntegral value)]

-- | @- filter@
filter_ :: IsCATransition caTransition => caTransition -> IO RawId
filter_ caTransition  =
  fmap (RawId . castPtr) $ sendMsg caTransition (mkSelector "filter") (retPtr retVoid) []

-- | @- setFilter:@
setFilter :: IsCATransition caTransition => caTransition -> RawId -> IO ()
setFilter caTransition  value =
  sendMsg caTransition (mkSelector "setFilter:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @subtype@
subtypeSelector :: Selector
subtypeSelector = mkSelector "subtype"

-- | @Selector@ for @setSubtype:@
setSubtypeSelector :: Selector
setSubtypeSelector = mkSelector "setSubtype:"

-- | @Selector@ for @startProgress@
startProgressSelector :: Selector
startProgressSelector = mkSelector "startProgress"

-- | @Selector@ for @setStartProgress:@
setStartProgressSelector :: Selector
setStartProgressSelector = mkSelector "setStartProgress:"

-- | @Selector@ for @endProgress@
endProgressSelector :: Selector
endProgressSelector = mkSelector "endProgress"

-- | @Selector@ for @setEndProgress:@
setEndProgressSelector :: Selector
setEndProgressSelector = mkSelector "setEndProgress:"

-- | @Selector@ for @filter@
filterSelector :: Selector
filterSelector = mkSelector "filter"

-- | @Selector@ for @setFilter:@
setFilterSelector :: Selector
setFilterSelector = mkSelector "setFilter:"

