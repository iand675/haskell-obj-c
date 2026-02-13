{-# LANGUAGE DataKinds #-}
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
  , endProgressSelector
  , filterSelector
  , setEndProgressSelector
  , setFilterSelector
  , setStartProgressSelector
  , setSubtypeSelector
  , setTypeSelector
  , startProgressSelector
  , subtypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsCATransition caTransition => caTransition -> IO (Id NSString)
type_ caTransition =
  sendMessage caTransition typeSelector

-- | @- setType:@
setType :: (IsCATransition caTransition, IsNSString value) => caTransition -> value -> IO ()
setType caTransition value =
  sendMessage caTransition setTypeSelector (toNSString value)

-- | @- subtype@
subtype :: IsCATransition caTransition => caTransition -> IO (Id NSString)
subtype caTransition =
  sendMessage caTransition subtypeSelector

-- | @- setSubtype:@
setSubtype :: (IsCATransition caTransition, IsNSString value) => caTransition -> value -> IO ()
setSubtype caTransition value =
  sendMessage caTransition setSubtypeSelector (toNSString value)

-- | @- startProgress@
startProgress :: IsCATransition caTransition => caTransition -> IO CFloat
startProgress caTransition =
  sendMessage caTransition startProgressSelector

-- | @- setStartProgress:@
setStartProgress :: IsCATransition caTransition => caTransition -> CFloat -> IO ()
setStartProgress caTransition value =
  sendMessage caTransition setStartProgressSelector value

-- | @- endProgress@
endProgress :: IsCATransition caTransition => caTransition -> IO CFloat
endProgress caTransition =
  sendMessage caTransition endProgressSelector

-- | @- setEndProgress:@
setEndProgress :: IsCATransition caTransition => caTransition -> CFloat -> IO ()
setEndProgress caTransition value =
  sendMessage caTransition setEndProgressSelector value

-- | @- filter@
filter_ :: IsCATransition caTransition => caTransition -> IO RawId
filter_ caTransition =
  sendMessage caTransition filterSelector

-- | @- setFilter:@
setFilter :: IsCATransition caTransition => caTransition -> RawId -> IO ()
setFilter caTransition value =
  sendMessage caTransition setFilterSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @subtype@
subtypeSelector :: Selector '[] (Id NSString)
subtypeSelector = mkSelector "subtype"

-- | @Selector@ for @setSubtype:@
setSubtypeSelector :: Selector '[Id NSString] ()
setSubtypeSelector = mkSelector "setSubtype:"

-- | @Selector@ for @startProgress@
startProgressSelector :: Selector '[] CFloat
startProgressSelector = mkSelector "startProgress"

-- | @Selector@ for @setStartProgress:@
setStartProgressSelector :: Selector '[CFloat] ()
setStartProgressSelector = mkSelector "setStartProgress:"

-- | @Selector@ for @endProgress@
endProgressSelector :: Selector '[] CFloat
endProgressSelector = mkSelector "endProgress"

-- | @Selector@ for @setEndProgress:@
setEndProgressSelector :: Selector '[CFloat] ()
setEndProgressSelector = mkSelector "setEndProgress:"

-- | @Selector@ for @filter@
filterSelector :: Selector '[] RawId
filterSelector = mkSelector "filter"

-- | @Selector@ for @setFilter:@
setFilterSelector :: Selector '[RawId] ()
setFilterSelector = mkSelector "setFilter:"

