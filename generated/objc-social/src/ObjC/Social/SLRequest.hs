{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SLRequest@.
module ObjC.Social.SLRequest
  ( SLRequest
  , IsSLRequest(..)
  , requestForServiceType_requestMethod_URL_parameters
  , addMultipartData_withName_type_filename
  , addMultipartData_withName_type
  , preparedURLRequest
  , performRequestWithHandler
  , requestMethod
  , url
  , parameters
  , requestForServiceType_requestMethod_URL_parametersSelector
  , addMultipartData_withName_type_filenameSelector
  , addMultipartData_withName_typeSelector
  , preparedURLRequestSelector
  , performRequestWithHandlerSelector
  , requestMethodSelector
  , urlSelector
  , parametersSelector

  -- * Enum types
  , SLRequestMethod(SLRequestMethod)
  , pattern SLRequestMethodGET
  , pattern SLRequestMethodPOST
  , pattern SLRequestMethodDELETE
  , pattern SLRequestMethodPUT

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

import ObjC.Social.Internal.Classes
import ObjC.Social.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ requestForServiceType:requestMethod:URL:parameters:@
requestForServiceType_requestMethod_URL_parameters :: (IsNSString serviceType, IsNSURL url, IsNSDictionary parameters) => serviceType -> SLRequestMethod -> url -> parameters -> IO (Id SLRequest)
requestForServiceType_requestMethod_URL_parameters serviceType requestMethod url parameters =
  do
    cls' <- getRequiredClass "SLRequest"
    withObjCPtr serviceType $ \raw_serviceType ->
      withObjCPtr url $ \raw_url ->
        withObjCPtr parameters $ \raw_parameters ->
          sendClassMsg cls' (mkSelector "requestForServiceType:requestMethod:URL:parameters:") (retPtr retVoid) [argPtr (castPtr raw_serviceType :: Ptr ()), argCLong (coerce requestMethod), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_parameters :: Ptr ())] >>= retainedObject . castPtr

-- | @- addMultipartData:withName:type:filename:@
addMultipartData_withName_type_filename :: (IsSLRequest slRequest, IsNSData data_, IsNSString name, IsNSString type_, IsNSString filename) => slRequest -> data_ -> name -> type_ -> filename -> IO ()
addMultipartData_withName_type_filename slRequest  data_ name type_ filename =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr filename $ \raw_filename ->
          sendMsg slRequest (mkSelector "addMultipartData:withName:type:filename:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_filename :: Ptr ())]

-- | @- addMultipartData:withName:type:@
addMultipartData_withName_type :: (IsSLRequest slRequest, IsNSData data_, IsNSString name, IsNSString type_) => slRequest -> data_ -> name -> type_ -> IO ()
addMultipartData_withName_type slRequest  data_ name type_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr type_ $ \raw_type_ ->
        sendMsg slRequest (mkSelector "addMultipartData:withName:type:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- preparedURLRequest@
preparedURLRequest :: IsSLRequest slRequest => slRequest -> IO (Id NSURLRequest)
preparedURLRequest slRequest  =
  sendMsg slRequest (mkSelector "preparedURLRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- performRequestWithHandler:@
performRequestWithHandler :: IsSLRequest slRequest => slRequest -> Ptr () -> IO ()
performRequestWithHandler slRequest  handler =
  sendMsg slRequest (mkSelector "performRequestWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- requestMethod@
requestMethod :: IsSLRequest slRequest => slRequest -> IO SLRequestMethod
requestMethod slRequest  =
  fmap (coerce :: CLong -> SLRequestMethod) $ sendMsg slRequest (mkSelector "requestMethod") retCLong []

-- | @- URL@
url :: IsSLRequest slRequest => slRequest -> IO (Id NSURL)
url slRequest  =
  sendMsg slRequest (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- parameters@
parameters :: IsSLRequest slRequest => slRequest -> IO (Id NSDictionary)
parameters slRequest  =
  sendMsg slRequest (mkSelector "parameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestForServiceType:requestMethod:URL:parameters:@
requestForServiceType_requestMethod_URL_parametersSelector :: Selector
requestForServiceType_requestMethod_URL_parametersSelector = mkSelector "requestForServiceType:requestMethod:URL:parameters:"

-- | @Selector@ for @addMultipartData:withName:type:filename:@
addMultipartData_withName_type_filenameSelector :: Selector
addMultipartData_withName_type_filenameSelector = mkSelector "addMultipartData:withName:type:filename:"

-- | @Selector@ for @addMultipartData:withName:type:@
addMultipartData_withName_typeSelector :: Selector
addMultipartData_withName_typeSelector = mkSelector "addMultipartData:withName:type:"

-- | @Selector@ for @preparedURLRequest@
preparedURLRequestSelector :: Selector
preparedURLRequestSelector = mkSelector "preparedURLRequest"

-- | @Selector@ for @performRequestWithHandler:@
performRequestWithHandlerSelector :: Selector
performRequestWithHandlerSelector = mkSelector "performRequestWithHandler:"

-- | @Selector@ for @requestMethod@
requestMethodSelector :: Selector
requestMethodSelector = mkSelector "requestMethod"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @parameters@
parametersSelector :: Selector
parametersSelector = mkSelector "parameters"

