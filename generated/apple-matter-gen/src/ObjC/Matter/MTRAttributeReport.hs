{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeReport@.
module ObjC.Matter.MTRAttributeReport
  ( MTRAttributeReport
  , IsMTRAttributeReport(..)
  , initWithResponseValue_error
  , path
  , value
  , error_
  , errorSelector
  , initWithResponseValue_errorSelector
  , pathSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRAttributeReport with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not an attribute response.
--
-- Will set the value property to nil and the error property to non-nil, even if the schema for the value is not known, if the response-value is an error, not data.
--
-- Will return nil and hand out an error if the response-value is data in the following cases:
--
-- * The response is for a cluster/attribute combination for which the schema is   unknown and hence the type of the data is not known. * The data does not match the known schema.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRAttributeReport mtrAttributeReport, IsNSDictionary responseValue, IsNSError error_) => mtrAttributeReport -> responseValue -> error_ -> IO (Id MTRAttributeReport)
initWithResponseValue_error mtrAttributeReport responseValue error_ =
  sendOwnedMessage mtrAttributeReport initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- path@
path :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO (Id MTRAttributePath)
path mtrAttributeReport =
  sendMessage mtrAttributeReport pathSelector

-- | value will be nil in the following cases:
--
-- * There was an error.  In this case, "error" will not be nil. * The attribute is nullable and the value of the attribute is null.
--
-- If value is not nil, the actual type of value will depend on the schema-defined (typically defined in the Matter specification) type of the attribute as follows:
--
-- * list: NSArray of whatever type the list entries are. * struct: The corresponding structure interface defined by Matter.framework * octet string: NSData * string: NSString * discrete/analog types: NSNumber
--
-- Derived types (in the Matter specification sense) are represented the same as the base type, except for "string" (which is a derived type of "octet string" in the specification).
--
-- ObjC selector: @- value@
value :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO RawId
value mtrAttributeReport =
  sendMessage mtrAttributeReport valueSelector

-- | If this specific path resulted in an error, the error (in the MTRInteractionErrorDomain or MTRErrorDomain) that corresponds to this path.
--
-- ObjC selector: @- error@
error_ :: IsMTRAttributeReport mtrAttributeReport => mtrAttributeReport -> IO (Id NSError)
error_ mtrAttributeReport =
  sendMessage mtrAttributeReport errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRAttributeReport)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id MTRAttributePath)
pathSelector = mkSelector "path"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

