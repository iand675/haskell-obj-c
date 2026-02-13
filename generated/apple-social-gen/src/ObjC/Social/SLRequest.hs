{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addMultipartData_withName_typeSelector
  , addMultipartData_withName_type_filenameSelector
  , parametersSelector
  , performRequestWithHandlerSelector
  , preparedURLRequestSelector
  , requestForServiceType_requestMethod_URL_parametersSelector
  , requestMethodSelector
  , urlSelector

  -- * Enum types
  , SLRequestMethod(SLRequestMethod)
  , pattern SLRequestMethodGET
  , pattern SLRequestMethodPOST
  , pattern SLRequestMethodDELETE
  , pattern SLRequestMethodPUT

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' requestForServiceType_requestMethod_URL_parametersSelector (toNSString serviceType) requestMethod (toNSURL url) (toNSDictionary parameters)

-- | @- addMultipartData:withName:type:filename:@
addMultipartData_withName_type_filename :: (IsSLRequest slRequest, IsNSData data_, IsNSString name, IsNSString type_, IsNSString filename) => slRequest -> data_ -> name -> type_ -> filename -> IO ()
addMultipartData_withName_type_filename slRequest data_ name type_ filename =
  sendMessage slRequest addMultipartData_withName_type_filenameSelector (toNSData data_) (toNSString name) (toNSString type_) (toNSString filename)

-- | @- addMultipartData:withName:type:@
addMultipartData_withName_type :: (IsSLRequest slRequest, IsNSData data_, IsNSString name, IsNSString type_) => slRequest -> data_ -> name -> type_ -> IO ()
addMultipartData_withName_type slRequest data_ name type_ =
  sendMessage slRequest addMultipartData_withName_typeSelector (toNSData data_) (toNSString name) (toNSString type_)

-- | @- preparedURLRequest@
preparedURLRequest :: IsSLRequest slRequest => slRequest -> IO (Id NSURLRequest)
preparedURLRequest slRequest =
  sendMessage slRequest preparedURLRequestSelector

-- | @- performRequestWithHandler:@
performRequestWithHandler :: IsSLRequest slRequest => slRequest -> Ptr () -> IO ()
performRequestWithHandler slRequest handler =
  sendMessage slRequest performRequestWithHandlerSelector handler

-- | @- requestMethod@
requestMethod :: IsSLRequest slRequest => slRequest -> IO SLRequestMethod
requestMethod slRequest =
  sendMessage slRequest requestMethodSelector

-- | @- URL@
url :: IsSLRequest slRequest => slRequest -> IO (Id NSURL)
url slRequest =
  sendMessage slRequest urlSelector

-- | @- parameters@
parameters :: IsSLRequest slRequest => slRequest -> IO (Id NSDictionary)
parameters slRequest =
  sendMessage slRequest parametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestForServiceType:requestMethod:URL:parameters:@
requestForServiceType_requestMethod_URL_parametersSelector :: Selector '[Id NSString, SLRequestMethod, Id NSURL, Id NSDictionary] (Id SLRequest)
requestForServiceType_requestMethod_URL_parametersSelector = mkSelector "requestForServiceType:requestMethod:URL:parameters:"

-- | @Selector@ for @addMultipartData:withName:type:filename:@
addMultipartData_withName_type_filenameSelector :: Selector '[Id NSData, Id NSString, Id NSString, Id NSString] ()
addMultipartData_withName_type_filenameSelector = mkSelector "addMultipartData:withName:type:filename:"

-- | @Selector@ for @addMultipartData:withName:type:@
addMultipartData_withName_typeSelector :: Selector '[Id NSData, Id NSString, Id NSString] ()
addMultipartData_withName_typeSelector = mkSelector "addMultipartData:withName:type:"

-- | @Selector@ for @preparedURLRequest@
preparedURLRequestSelector :: Selector '[] (Id NSURLRequest)
preparedURLRequestSelector = mkSelector "preparedURLRequest"

-- | @Selector@ for @performRequestWithHandler:@
performRequestWithHandlerSelector :: Selector '[Ptr ()] ()
performRequestWithHandlerSelector = mkSelector "performRequestWithHandler:"

-- | @Selector@ for @requestMethod@
requestMethodSelector :: Selector '[] SLRequestMethod
requestMethodSelector = mkSelector "requestMethod"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @parameters@
parametersSelector :: Selector '[] (Id NSDictionary)
parametersSelector = mkSelector "parameters"

