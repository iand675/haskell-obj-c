{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLComponents@.
module ObjC.Foundation.NSURLComponents
  ( NSURLComponents
  , IsNSURLComponents(..)
  , init_
  , initWithURL_resolvingAgainstBaseURL
  , componentsWithURL_resolvingAgainstBaseURL
  , initWithString
  , componentsWithString
  , initWithString_encodingInvalidCharacters
  , componentsWithString_encodingInvalidCharacters
  , urlRelativeToURL
  , url
  , string
  , scheme
  , setScheme
  , user
  , setUser
  , password
  , setPassword
  , host
  , setHost
  , port
  , setPort
  , path
  , setPath
  , query
  , setQuery
  , fragment
  , setFragment
  , percentEncodedUser
  , setPercentEncodedUser
  , percentEncodedPassword
  , setPercentEncodedPassword
  , percentEncodedHost
  , setPercentEncodedHost
  , percentEncodedPath
  , setPercentEncodedPath
  , percentEncodedQuery
  , setPercentEncodedQuery
  , percentEncodedFragment
  , setPercentEncodedFragment
  , encodedHost
  , setEncodedHost
  , rangeOfScheme
  , rangeOfUser
  , rangeOfPassword
  , rangeOfHost
  , rangeOfPort
  , rangeOfPath
  , rangeOfQuery
  , rangeOfFragment
  , queryItems
  , setQueryItems
  , percentEncodedQueryItems
  , setPercentEncodedQueryItems
  , initSelector
  , initWithURL_resolvingAgainstBaseURLSelector
  , componentsWithURL_resolvingAgainstBaseURLSelector
  , initWithStringSelector
  , componentsWithStringSelector
  , initWithString_encodingInvalidCharactersSelector
  , componentsWithString_encodingInvalidCharactersSelector
  , urlRelativeToURLSelector
  , urlSelector
  , stringSelector
  , schemeSelector
  , setSchemeSelector
  , userSelector
  , setUserSelector
  , passwordSelector
  , setPasswordSelector
  , hostSelector
  , setHostSelector
  , portSelector
  , setPortSelector
  , pathSelector
  , setPathSelector
  , querySelector
  , setQuerySelector
  , fragmentSelector
  , setFragmentSelector
  , percentEncodedUserSelector
  , setPercentEncodedUserSelector
  , percentEncodedPasswordSelector
  , setPercentEncodedPasswordSelector
  , percentEncodedHostSelector
  , setPercentEncodedHostSelector
  , percentEncodedPathSelector
  , setPercentEncodedPathSelector
  , percentEncodedQuerySelector
  , setPercentEncodedQuerySelector
  , percentEncodedFragmentSelector
  , setPercentEncodedFragmentSelector
  , encodedHostSelector
  , setEncodedHostSelector
  , rangeOfSchemeSelector
  , rangeOfUserSelector
  , rangeOfPasswordSelector
  , rangeOfHostSelector
  , rangeOfPortSelector
  , rangeOfPathSelector
  , rangeOfQuerySelector
  , rangeOfFragmentSelector
  , queryItemsSelector
  , setQueryItemsSelector
  , percentEncodedQueryItemsSelector
  , setPercentEncodedQueryItemsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- init@
init_ :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSURLComponents)
init_ nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithURL:resolvingAgainstBaseURL:@
initWithURL_resolvingAgainstBaseURL :: (IsNSURLComponents nsurlComponents, IsNSURL url) => nsurlComponents -> url -> Bool -> IO (Id NSURLComponents)
initWithURL_resolvingAgainstBaseURL nsurlComponents  url resolve =
withObjCPtr url $ \raw_url ->
    sendMsg nsurlComponents (mkSelector "initWithURL:resolvingAgainstBaseURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if resolve then 1 else 0)] >>= ownedObject . castPtr

-- | @+ componentsWithURL:resolvingAgainstBaseURL:@
componentsWithURL_resolvingAgainstBaseURL :: IsNSURL url => url -> Bool -> IO (Id NSURLComponents)
componentsWithURL_resolvingAgainstBaseURL url resolve =
  do
    cls' <- getRequiredClass "NSURLComponents"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "componentsWithURL:resolvingAgainstBaseURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if resolve then 1 else 0)] >>= retainedObject . castPtr

-- | @- initWithString:@
initWithString :: (IsNSURLComponents nsurlComponents, IsNSString urlString) => nsurlComponents -> urlString -> IO (Id NSURLComponents)
initWithString nsurlComponents  urlString =
withObjCPtr urlString $ \raw_urlString ->
    sendMsg nsurlComponents (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ())] >>= ownedObject . castPtr

-- | @+ componentsWithString:@
componentsWithString :: IsNSString urlString => urlString -> IO (Id NSURLComponents)
componentsWithString urlString =
  do
    cls' <- getRequiredClass "NSURLComponents"
    withObjCPtr urlString $ \raw_urlString ->
      sendClassMsg cls' (mkSelector "componentsWithString:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ())] >>= retainedObject . castPtr

-- | Initializes an @NSURLComponents@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURLComponents@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURLComponents@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURLComponents@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @- initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharacters :: (IsNSURLComponents nsurlComponents, IsNSString urlString) => nsurlComponents -> urlString -> Bool -> IO (Id NSURLComponents)
initWithString_encodingInvalidCharacters nsurlComponents  urlString encodingInvalidCharacters =
withObjCPtr urlString $ \raw_urlString ->
    sendMsg nsurlComponents (mkSelector "initWithString:encodingInvalidCharacters:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argCULong (if encodingInvalidCharacters then 1 else 0)] >>= ownedObject . castPtr

-- | Initializes and returns a newly created @NSURLComponents@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURLComponents@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, nil is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURLComponents@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURLComponents@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @+ componentsWithString:encodingInvalidCharacters:@
componentsWithString_encodingInvalidCharacters :: IsNSString urlString => urlString -> Bool -> IO (Id NSURLComponents)
componentsWithString_encodingInvalidCharacters urlString encodingInvalidCharacters =
  do
    cls' <- getRequiredClass "NSURLComponents"
    withObjCPtr urlString $ \raw_urlString ->
      sendClassMsg cls' (mkSelector "componentsWithString:encodingInvalidCharacters:") (retPtr retVoid) [argPtr (castPtr raw_urlString :: Ptr ()), argCULong (if encodingInvalidCharacters then 1 else 0)] >>= retainedObject . castPtr

-- | @- URLRelativeToURL:@
urlRelativeToURL :: (IsNSURLComponents nsurlComponents, IsNSURL baseURL) => nsurlComponents -> baseURL -> IO (Id NSURL)
urlRelativeToURL nsurlComponents  baseURL =
withObjCPtr baseURL $ \raw_baseURL ->
    sendMsg nsurlComponents (mkSelector "URLRelativeToURL:") (retPtr retVoid) [argPtr (castPtr raw_baseURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- URL@
url :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSURL)
url nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- string@
string :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
string nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scheme@
scheme :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
scheme nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "scheme") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScheme:@
setScheme :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setScheme nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setScheme:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- user@
user :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
user nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUser:@
setUser :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setUser nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setUser:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- password@
password :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
password nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "password") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPassword:@
setPassword :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPassword nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- host@
host :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
host nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHost:@
setHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setHost nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setHost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- port@
port :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSNumber)
port nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "port") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPort:@
setPort :: (IsNSURLComponents nsurlComponents, IsNSNumber value) => nsurlComponents -> value -> IO ()
setPort nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPort:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- path@
path :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
path nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPath:@
setPath :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPath nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- query@
query :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
query nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "query") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuery:@
setQuery :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setQuery nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setQuery:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fragment@
fragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
fragment nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "fragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFragment:@
setFragment :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setFragment nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setFragment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedUser@
percentEncodedUser :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedUser nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedUser:@
setPercentEncodedUser :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedUser nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedUser:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedPassword@
percentEncodedPassword :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedPassword nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedPassword") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedPassword:@
setPercentEncodedPassword :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedPassword nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedHost@
percentEncodedHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedHost nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedHost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedHost:@
setPercentEncodedHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedHost nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedHost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedPath@
percentEncodedPath :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedPath nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedPath:@
setPercentEncodedPath :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedPath nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedQuery@
percentEncodedQuery :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedQuery nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedQuery") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedQuery:@
setPercentEncodedQuery :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedQuery nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedQuery:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedFragment@
percentEncodedFragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedFragment nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedFragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedFragment:@
setPercentEncodedFragment :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedFragment nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedFragment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encodedHost@
encodedHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
encodedHost nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "encodedHost") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncodedHost:@
setEncodedHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setEncodedHost nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setEncodedHost:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- rangeOfScheme@
rangeOfScheme :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfScheme nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfScheme") retNSRange []

-- | @- rangeOfUser@
rangeOfUser :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfUser nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfUser") retNSRange []

-- | @- rangeOfPassword@
rangeOfPassword :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPassword nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfPassword") retNSRange []

-- | @- rangeOfHost@
rangeOfHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfHost nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfHost") retNSRange []

-- | @- rangeOfPort@
rangeOfPort :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPort nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfPort") retNSRange []

-- | @- rangeOfPath@
rangeOfPath :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPath nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfPath") retNSRange []

-- | @- rangeOfQuery@
rangeOfQuery :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfQuery nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfQuery") retNSRange []

-- | @- rangeOfFragment@
rangeOfFragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfFragment nsurlComponents  =
  sendMsgStret nsurlComponents (mkSelector "rangeOfFragment") retNSRange []

-- | @- queryItems@
queryItems :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSArray)
queryItems nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "queryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQueryItems:@
setQueryItems :: (IsNSURLComponents nsurlComponents, IsNSArray value) => nsurlComponents -> value -> IO ()
setQueryItems nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setQueryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- percentEncodedQueryItems@
percentEncodedQueryItems :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSArray)
percentEncodedQueryItems nsurlComponents  =
  sendMsg nsurlComponents (mkSelector "percentEncodedQueryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPercentEncodedQueryItems:@
setPercentEncodedQueryItems :: (IsNSURLComponents nsurlComponents, IsNSArray value) => nsurlComponents -> value -> IO ()
setPercentEncodedQueryItems nsurlComponents  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsurlComponents (mkSelector "setPercentEncodedQueryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:resolvingAgainstBaseURL:@
initWithURL_resolvingAgainstBaseURLSelector :: Selector
initWithURL_resolvingAgainstBaseURLSelector = mkSelector "initWithURL:resolvingAgainstBaseURL:"

-- | @Selector@ for @componentsWithURL:resolvingAgainstBaseURL:@
componentsWithURL_resolvingAgainstBaseURLSelector :: Selector
componentsWithURL_resolvingAgainstBaseURLSelector = mkSelector "componentsWithURL:resolvingAgainstBaseURL:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @componentsWithString:@
componentsWithStringSelector :: Selector
componentsWithStringSelector = mkSelector "componentsWithString:"

-- | @Selector@ for @initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharactersSelector :: Selector
initWithString_encodingInvalidCharactersSelector = mkSelector "initWithString:encodingInvalidCharacters:"

-- | @Selector@ for @componentsWithString:encodingInvalidCharacters:@
componentsWithString_encodingInvalidCharactersSelector :: Selector
componentsWithString_encodingInvalidCharactersSelector = mkSelector "componentsWithString:encodingInvalidCharacters:"

-- | @Selector@ for @URLRelativeToURL:@
urlRelativeToURLSelector :: Selector
urlRelativeToURLSelector = mkSelector "URLRelativeToURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @scheme@
schemeSelector :: Selector
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @setScheme:@
setSchemeSelector :: Selector
setSchemeSelector = mkSelector "setScheme:"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @setUser:@
setUserSelector :: Selector
setUserSelector = mkSelector "setUser:"

-- | @Selector@ for @password@
passwordSelector :: Selector
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @setHost:@
setHostSelector :: Selector
setHostSelector = mkSelector "setHost:"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @query@
querySelector :: Selector
querySelector = mkSelector "query"

-- | @Selector@ for @setQuery:@
setQuerySelector :: Selector
setQuerySelector = mkSelector "setQuery:"

-- | @Selector@ for @fragment@
fragmentSelector :: Selector
fragmentSelector = mkSelector "fragment"

-- | @Selector@ for @setFragment:@
setFragmentSelector :: Selector
setFragmentSelector = mkSelector "setFragment:"

-- | @Selector@ for @percentEncodedUser@
percentEncodedUserSelector :: Selector
percentEncodedUserSelector = mkSelector "percentEncodedUser"

-- | @Selector@ for @setPercentEncodedUser:@
setPercentEncodedUserSelector :: Selector
setPercentEncodedUserSelector = mkSelector "setPercentEncodedUser:"

-- | @Selector@ for @percentEncodedPassword@
percentEncodedPasswordSelector :: Selector
percentEncodedPasswordSelector = mkSelector "percentEncodedPassword"

-- | @Selector@ for @setPercentEncodedPassword:@
setPercentEncodedPasswordSelector :: Selector
setPercentEncodedPasswordSelector = mkSelector "setPercentEncodedPassword:"

-- | @Selector@ for @percentEncodedHost@
percentEncodedHostSelector :: Selector
percentEncodedHostSelector = mkSelector "percentEncodedHost"

-- | @Selector@ for @setPercentEncodedHost:@
setPercentEncodedHostSelector :: Selector
setPercentEncodedHostSelector = mkSelector "setPercentEncodedHost:"

-- | @Selector@ for @percentEncodedPath@
percentEncodedPathSelector :: Selector
percentEncodedPathSelector = mkSelector "percentEncodedPath"

-- | @Selector@ for @setPercentEncodedPath:@
setPercentEncodedPathSelector :: Selector
setPercentEncodedPathSelector = mkSelector "setPercentEncodedPath:"

-- | @Selector@ for @percentEncodedQuery@
percentEncodedQuerySelector :: Selector
percentEncodedQuerySelector = mkSelector "percentEncodedQuery"

-- | @Selector@ for @setPercentEncodedQuery:@
setPercentEncodedQuerySelector :: Selector
setPercentEncodedQuerySelector = mkSelector "setPercentEncodedQuery:"

-- | @Selector@ for @percentEncodedFragment@
percentEncodedFragmentSelector :: Selector
percentEncodedFragmentSelector = mkSelector "percentEncodedFragment"

-- | @Selector@ for @setPercentEncodedFragment:@
setPercentEncodedFragmentSelector :: Selector
setPercentEncodedFragmentSelector = mkSelector "setPercentEncodedFragment:"

-- | @Selector@ for @encodedHost@
encodedHostSelector :: Selector
encodedHostSelector = mkSelector "encodedHost"

-- | @Selector@ for @setEncodedHost:@
setEncodedHostSelector :: Selector
setEncodedHostSelector = mkSelector "setEncodedHost:"

-- | @Selector@ for @rangeOfScheme@
rangeOfSchemeSelector :: Selector
rangeOfSchemeSelector = mkSelector "rangeOfScheme"

-- | @Selector@ for @rangeOfUser@
rangeOfUserSelector :: Selector
rangeOfUserSelector = mkSelector "rangeOfUser"

-- | @Selector@ for @rangeOfPassword@
rangeOfPasswordSelector :: Selector
rangeOfPasswordSelector = mkSelector "rangeOfPassword"

-- | @Selector@ for @rangeOfHost@
rangeOfHostSelector :: Selector
rangeOfHostSelector = mkSelector "rangeOfHost"

-- | @Selector@ for @rangeOfPort@
rangeOfPortSelector :: Selector
rangeOfPortSelector = mkSelector "rangeOfPort"

-- | @Selector@ for @rangeOfPath@
rangeOfPathSelector :: Selector
rangeOfPathSelector = mkSelector "rangeOfPath"

-- | @Selector@ for @rangeOfQuery@
rangeOfQuerySelector :: Selector
rangeOfQuerySelector = mkSelector "rangeOfQuery"

-- | @Selector@ for @rangeOfFragment@
rangeOfFragmentSelector :: Selector
rangeOfFragmentSelector = mkSelector "rangeOfFragment"

-- | @Selector@ for @queryItems@
queryItemsSelector :: Selector
queryItemsSelector = mkSelector "queryItems"

-- | @Selector@ for @setQueryItems:@
setQueryItemsSelector :: Selector
setQueryItemsSelector = mkSelector "setQueryItems:"

-- | @Selector@ for @percentEncodedQueryItems@
percentEncodedQueryItemsSelector :: Selector
percentEncodedQueryItemsSelector = mkSelector "percentEncodedQueryItems"

-- | @Selector@ for @setPercentEncodedQueryItems:@
setPercentEncodedQueryItemsSelector :: Selector
setPercentEncodedQueryItemsSelector = mkSelector "setPercentEncodedQueryItems:"

