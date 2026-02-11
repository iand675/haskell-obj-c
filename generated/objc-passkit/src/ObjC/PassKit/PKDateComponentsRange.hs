{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKDateComponentsRange@.
module ObjC.PassKit.PKDateComponentsRange
  ( PKDateComponentsRange
  , IsPKDateComponentsRange(..)
  , init_
  , initWithStartDateComponents_endDateComponents
  , startDateComponents
  , endDateComponents
  , initSelector
  , initWithStartDateComponents_endDateComponentsSelector
  , startDateComponentsSelector
  , endDateComponentsSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id PKDateComponentsRange)
init_ pkDateComponentsRange  =
  sendMsg pkDateComponentsRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponents :: (IsPKDateComponentsRange pkDateComponentsRange, IsNSDateComponents startDateComponents, IsNSDateComponents endDateComponents) => pkDateComponentsRange -> startDateComponents -> endDateComponents -> IO (Id PKDateComponentsRange)
initWithStartDateComponents_endDateComponents pkDateComponentsRange  startDateComponents endDateComponents =
withObjCPtr startDateComponents $ \raw_startDateComponents ->
  withObjCPtr endDateComponents $ \raw_endDateComponents ->
      sendMsg pkDateComponentsRange (mkSelector "initWithStartDateComponents:endDateComponents:") (retPtr retVoid) [argPtr (castPtr raw_startDateComponents :: Ptr ()), argPtr (castPtr raw_endDateComponents :: Ptr ())] >>= ownedObject . castPtr

-- | @- startDateComponents@
startDateComponents :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id NSDateComponents)
startDateComponents pkDateComponentsRange  =
  sendMsg pkDateComponentsRange (mkSelector "startDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endDateComponents@
endDateComponents :: IsPKDateComponentsRange pkDateComponentsRange => pkDateComponentsRange -> IO (Id NSDateComponents)
endDateComponents pkDateComponentsRange  =
  sendMsg pkDateComponentsRange (mkSelector "endDateComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithStartDateComponents:endDateComponents:@
initWithStartDateComponents_endDateComponentsSelector :: Selector
initWithStartDateComponents_endDateComponentsSelector = mkSelector "initWithStartDateComponents:endDateComponents:"

-- | @Selector@ for @startDateComponents@
startDateComponentsSelector :: Selector
startDateComponentsSelector = mkSelector "startDateComponents"

-- | @Selector@ for @endDateComponents@
endDateComponentsSelector :: Selector
endDateComponentsSelector = mkSelector "endDateComponents"

