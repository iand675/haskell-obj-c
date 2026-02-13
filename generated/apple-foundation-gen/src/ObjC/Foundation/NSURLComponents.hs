{-# LANGUAGE DataKinds #-}
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
  , componentsWithStringSelector
  , componentsWithString_encodingInvalidCharactersSelector
  , componentsWithURL_resolvingAgainstBaseURLSelector
  , encodedHostSelector
  , fragmentSelector
  , hostSelector
  , initSelector
  , initWithStringSelector
  , initWithString_encodingInvalidCharactersSelector
  , initWithURL_resolvingAgainstBaseURLSelector
  , passwordSelector
  , pathSelector
  , percentEncodedFragmentSelector
  , percentEncodedHostSelector
  , percentEncodedPasswordSelector
  , percentEncodedPathSelector
  , percentEncodedQueryItemsSelector
  , percentEncodedQuerySelector
  , percentEncodedUserSelector
  , portSelector
  , queryItemsSelector
  , querySelector
  , rangeOfFragmentSelector
  , rangeOfHostSelector
  , rangeOfPasswordSelector
  , rangeOfPathSelector
  , rangeOfPortSelector
  , rangeOfQuerySelector
  , rangeOfSchemeSelector
  , rangeOfUserSelector
  , schemeSelector
  , setEncodedHostSelector
  , setFragmentSelector
  , setHostSelector
  , setPasswordSelector
  , setPathSelector
  , setPercentEncodedFragmentSelector
  , setPercentEncodedHostSelector
  , setPercentEncodedPasswordSelector
  , setPercentEncodedPathSelector
  , setPercentEncodedQueryItemsSelector
  , setPercentEncodedQuerySelector
  , setPercentEncodedUserSelector
  , setPortSelector
  , setQueryItemsSelector
  , setQuerySelector
  , setSchemeSelector
  , setUserSelector
  , stringSelector
  , urlRelativeToURLSelector
  , urlSelector
  , userSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- init@
init_ :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSURLComponents)
init_ nsurlComponents =
  sendOwnedMessage nsurlComponents initSelector

-- | @- initWithURL:resolvingAgainstBaseURL:@
initWithURL_resolvingAgainstBaseURL :: (IsNSURLComponents nsurlComponents, IsNSURL url) => nsurlComponents -> url -> Bool -> IO (Id NSURLComponents)
initWithURL_resolvingAgainstBaseURL nsurlComponents url resolve =
  sendOwnedMessage nsurlComponents initWithURL_resolvingAgainstBaseURLSelector (toNSURL url) resolve

-- | @+ componentsWithURL:resolvingAgainstBaseURL:@
componentsWithURL_resolvingAgainstBaseURL :: IsNSURL url => url -> Bool -> IO (Id NSURLComponents)
componentsWithURL_resolvingAgainstBaseURL url resolve =
  do
    cls' <- getRequiredClass "NSURLComponents"
    sendClassMessage cls' componentsWithURL_resolvingAgainstBaseURLSelector (toNSURL url) resolve

-- | @- initWithString:@
initWithString :: (IsNSURLComponents nsurlComponents, IsNSString urlString) => nsurlComponents -> urlString -> IO (Id NSURLComponents)
initWithString nsurlComponents urlString =
  sendOwnedMessage nsurlComponents initWithStringSelector (toNSString urlString)

-- | @+ componentsWithString:@
componentsWithString :: IsNSString urlString => urlString -> IO (Id NSURLComponents)
componentsWithString urlString =
  do
    cls' <- getRequiredClass "NSURLComponents"
    sendClassMessage cls' componentsWithStringSelector (toNSString urlString)

-- | Initializes an @NSURLComponents@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURLComponents@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, @nil@ is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURLComponents@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURLComponents@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @- initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharacters :: (IsNSURLComponents nsurlComponents, IsNSString urlString) => nsurlComponents -> urlString -> Bool -> IO (Id NSURLComponents)
initWithString_encodingInvalidCharacters nsurlComponents urlString encodingInvalidCharacters =
  sendOwnedMessage nsurlComponents initWithString_encodingInvalidCharactersSelector (toNSString urlString) encodingInvalidCharacters

-- | Initializes and returns a newly created @NSURLComponents@ with a URL string and the option to add (or skip) IDNA- and percent-encoding of invalid characters. If @encodingInvalidCharacters@ is false, and the URL string is invalid according to RFC 3986, @nil@ is returned. If @encodingInvalidCharacters@ is true, @NSURLComponents@ will try to encode the string to create a valid URL. If the URL string is still invalid after encoding, nil is returned.
--
-- - Parameter URLString: The URL string. - Parameter encodingInvalidCharacters: True if @NSURLComponents@ should try to encode an invalid URL string, false otherwise. - Returns: An @NSURLComponents@ instance for a valid URL, or @nil@ if the URL is invalid.
--
-- ObjC selector: @+ componentsWithString:encodingInvalidCharacters:@
componentsWithString_encodingInvalidCharacters :: IsNSString urlString => urlString -> Bool -> IO (Id NSURLComponents)
componentsWithString_encodingInvalidCharacters urlString encodingInvalidCharacters =
  do
    cls' <- getRequiredClass "NSURLComponents"
    sendClassMessage cls' componentsWithString_encodingInvalidCharactersSelector (toNSString urlString) encodingInvalidCharacters

-- | @- URLRelativeToURL:@
urlRelativeToURL :: (IsNSURLComponents nsurlComponents, IsNSURL baseURL) => nsurlComponents -> baseURL -> IO (Id NSURL)
urlRelativeToURL nsurlComponents baseURL =
  sendMessage nsurlComponents urlRelativeToURLSelector (toNSURL baseURL)

-- | @- URL@
url :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSURL)
url nsurlComponents =
  sendMessage nsurlComponents urlSelector

-- | @- string@
string :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
string nsurlComponents =
  sendMessage nsurlComponents stringSelector

-- | @- scheme@
scheme :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
scheme nsurlComponents =
  sendMessage nsurlComponents schemeSelector

-- | @- setScheme:@
setScheme :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setScheme nsurlComponents value =
  sendMessage nsurlComponents setSchemeSelector (toNSString value)

-- | @- user@
user :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
user nsurlComponents =
  sendMessage nsurlComponents userSelector

-- | @- setUser:@
setUser :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setUser nsurlComponents value =
  sendMessage nsurlComponents setUserSelector (toNSString value)

-- | @- password@
password :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
password nsurlComponents =
  sendMessage nsurlComponents passwordSelector

-- | @- setPassword:@
setPassword :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPassword nsurlComponents value =
  sendMessage nsurlComponents setPasswordSelector (toNSString value)

-- | @- host@
host :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
host nsurlComponents =
  sendMessage nsurlComponents hostSelector

-- | @- setHost:@
setHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setHost nsurlComponents value =
  sendMessage nsurlComponents setHostSelector (toNSString value)

-- | @- port@
port :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSNumber)
port nsurlComponents =
  sendMessage nsurlComponents portSelector

-- | @- setPort:@
setPort :: (IsNSURLComponents nsurlComponents, IsNSNumber value) => nsurlComponents -> value -> IO ()
setPort nsurlComponents value =
  sendMessage nsurlComponents setPortSelector (toNSNumber value)

-- | @- path@
path :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
path nsurlComponents =
  sendMessage nsurlComponents pathSelector

-- | @- setPath:@
setPath :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPath nsurlComponents value =
  sendMessage nsurlComponents setPathSelector (toNSString value)

-- | @- query@
query :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
query nsurlComponents =
  sendMessage nsurlComponents querySelector

-- | @- setQuery:@
setQuery :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setQuery nsurlComponents value =
  sendMessage nsurlComponents setQuerySelector (toNSString value)

-- | @- fragment@
fragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
fragment nsurlComponents =
  sendMessage nsurlComponents fragmentSelector

-- | @- setFragment:@
setFragment :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setFragment nsurlComponents value =
  sendMessage nsurlComponents setFragmentSelector (toNSString value)

-- | @- percentEncodedUser@
percentEncodedUser :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedUser nsurlComponents =
  sendMessage nsurlComponents percentEncodedUserSelector

-- | @- setPercentEncodedUser:@
setPercentEncodedUser :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedUser nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedUserSelector (toNSString value)

-- | @- percentEncodedPassword@
percentEncodedPassword :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedPassword nsurlComponents =
  sendMessage nsurlComponents percentEncodedPasswordSelector

-- | @- setPercentEncodedPassword:@
setPercentEncodedPassword :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedPassword nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedPasswordSelector (toNSString value)

-- | @- percentEncodedHost@
percentEncodedHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedHost nsurlComponents =
  sendMessage nsurlComponents percentEncodedHostSelector

-- | @- setPercentEncodedHost:@
setPercentEncodedHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedHost nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedHostSelector (toNSString value)

-- | @- percentEncodedPath@
percentEncodedPath :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedPath nsurlComponents =
  sendMessage nsurlComponents percentEncodedPathSelector

-- | @- setPercentEncodedPath:@
setPercentEncodedPath :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedPath nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedPathSelector (toNSString value)

-- | @- percentEncodedQuery@
percentEncodedQuery :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedQuery nsurlComponents =
  sendMessage nsurlComponents percentEncodedQuerySelector

-- | @- setPercentEncodedQuery:@
setPercentEncodedQuery :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedQuery nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedQuerySelector (toNSString value)

-- | @- percentEncodedFragment@
percentEncodedFragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
percentEncodedFragment nsurlComponents =
  sendMessage nsurlComponents percentEncodedFragmentSelector

-- | @- setPercentEncodedFragment:@
setPercentEncodedFragment :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setPercentEncodedFragment nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedFragmentSelector (toNSString value)

-- | @- encodedHost@
encodedHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSString)
encodedHost nsurlComponents =
  sendMessage nsurlComponents encodedHostSelector

-- | @- setEncodedHost:@
setEncodedHost :: (IsNSURLComponents nsurlComponents, IsNSString value) => nsurlComponents -> value -> IO ()
setEncodedHost nsurlComponents value =
  sendMessage nsurlComponents setEncodedHostSelector (toNSString value)

-- | @- rangeOfScheme@
rangeOfScheme :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfScheme nsurlComponents =
  sendMessage nsurlComponents rangeOfSchemeSelector

-- | @- rangeOfUser@
rangeOfUser :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfUser nsurlComponents =
  sendMessage nsurlComponents rangeOfUserSelector

-- | @- rangeOfPassword@
rangeOfPassword :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPassword nsurlComponents =
  sendMessage nsurlComponents rangeOfPasswordSelector

-- | @- rangeOfHost@
rangeOfHost :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfHost nsurlComponents =
  sendMessage nsurlComponents rangeOfHostSelector

-- | @- rangeOfPort@
rangeOfPort :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPort nsurlComponents =
  sendMessage nsurlComponents rangeOfPortSelector

-- | @- rangeOfPath@
rangeOfPath :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfPath nsurlComponents =
  sendMessage nsurlComponents rangeOfPathSelector

-- | @- rangeOfQuery@
rangeOfQuery :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfQuery nsurlComponents =
  sendMessage nsurlComponents rangeOfQuerySelector

-- | @- rangeOfFragment@
rangeOfFragment :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO NSRange
rangeOfFragment nsurlComponents =
  sendMessage nsurlComponents rangeOfFragmentSelector

-- | @- queryItems@
queryItems :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSArray)
queryItems nsurlComponents =
  sendMessage nsurlComponents queryItemsSelector

-- | @- setQueryItems:@
setQueryItems :: (IsNSURLComponents nsurlComponents, IsNSArray value) => nsurlComponents -> value -> IO ()
setQueryItems nsurlComponents value =
  sendMessage nsurlComponents setQueryItemsSelector (toNSArray value)

-- | @- percentEncodedQueryItems@
percentEncodedQueryItems :: IsNSURLComponents nsurlComponents => nsurlComponents -> IO (Id NSArray)
percentEncodedQueryItems nsurlComponents =
  sendMessage nsurlComponents percentEncodedQueryItemsSelector

-- | @- setPercentEncodedQueryItems:@
setPercentEncodedQueryItems :: (IsNSURLComponents nsurlComponents, IsNSArray value) => nsurlComponents -> value -> IO ()
setPercentEncodedQueryItems nsurlComponents value =
  sendMessage nsurlComponents setPercentEncodedQueryItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSURLComponents)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:resolvingAgainstBaseURL:@
initWithURL_resolvingAgainstBaseURLSelector :: Selector '[Id NSURL, Bool] (Id NSURLComponents)
initWithURL_resolvingAgainstBaseURLSelector = mkSelector "initWithURL:resolvingAgainstBaseURL:"

-- | @Selector@ for @componentsWithURL:resolvingAgainstBaseURL:@
componentsWithURL_resolvingAgainstBaseURLSelector :: Selector '[Id NSURL, Bool] (Id NSURLComponents)
componentsWithURL_resolvingAgainstBaseURLSelector = mkSelector "componentsWithURL:resolvingAgainstBaseURL:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id NSURLComponents)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @componentsWithString:@
componentsWithStringSelector :: Selector '[Id NSString] (Id NSURLComponents)
componentsWithStringSelector = mkSelector "componentsWithString:"

-- | @Selector@ for @initWithString:encodingInvalidCharacters:@
initWithString_encodingInvalidCharactersSelector :: Selector '[Id NSString, Bool] (Id NSURLComponents)
initWithString_encodingInvalidCharactersSelector = mkSelector "initWithString:encodingInvalidCharacters:"

-- | @Selector@ for @componentsWithString:encodingInvalidCharacters:@
componentsWithString_encodingInvalidCharactersSelector :: Selector '[Id NSString, Bool] (Id NSURLComponents)
componentsWithString_encodingInvalidCharactersSelector = mkSelector "componentsWithString:encodingInvalidCharacters:"

-- | @Selector@ for @URLRelativeToURL:@
urlRelativeToURLSelector :: Selector '[Id NSURL] (Id NSURL)
urlRelativeToURLSelector = mkSelector "URLRelativeToURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @scheme@
schemeSelector :: Selector '[] (Id NSString)
schemeSelector = mkSelector "scheme"

-- | @Selector@ for @setScheme:@
setSchemeSelector :: Selector '[Id NSString] ()
setSchemeSelector = mkSelector "setScheme:"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @setUser:@
setUserSelector :: Selector '[Id NSString] ()
setUserSelector = mkSelector "setUser:"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @setHost:@
setHostSelector :: Selector '[Id NSString] ()
setHostSelector = mkSelector "setHost:"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSNumber)
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector '[Id NSNumber] ()
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSString] ()
setPathSelector = mkSelector "setPath:"

-- | @Selector@ for @query@
querySelector :: Selector '[] (Id NSString)
querySelector = mkSelector "query"

-- | @Selector@ for @setQuery:@
setQuerySelector :: Selector '[Id NSString] ()
setQuerySelector = mkSelector "setQuery:"

-- | @Selector@ for @fragment@
fragmentSelector :: Selector '[] (Id NSString)
fragmentSelector = mkSelector "fragment"

-- | @Selector@ for @setFragment:@
setFragmentSelector :: Selector '[Id NSString] ()
setFragmentSelector = mkSelector "setFragment:"

-- | @Selector@ for @percentEncodedUser@
percentEncodedUserSelector :: Selector '[] (Id NSString)
percentEncodedUserSelector = mkSelector "percentEncodedUser"

-- | @Selector@ for @setPercentEncodedUser:@
setPercentEncodedUserSelector :: Selector '[Id NSString] ()
setPercentEncodedUserSelector = mkSelector "setPercentEncodedUser:"

-- | @Selector@ for @percentEncodedPassword@
percentEncodedPasswordSelector :: Selector '[] (Id NSString)
percentEncodedPasswordSelector = mkSelector "percentEncodedPassword"

-- | @Selector@ for @setPercentEncodedPassword:@
setPercentEncodedPasswordSelector :: Selector '[Id NSString] ()
setPercentEncodedPasswordSelector = mkSelector "setPercentEncodedPassword:"

-- | @Selector@ for @percentEncodedHost@
percentEncodedHostSelector :: Selector '[] (Id NSString)
percentEncodedHostSelector = mkSelector "percentEncodedHost"

-- | @Selector@ for @setPercentEncodedHost:@
setPercentEncodedHostSelector :: Selector '[Id NSString] ()
setPercentEncodedHostSelector = mkSelector "setPercentEncodedHost:"

-- | @Selector@ for @percentEncodedPath@
percentEncodedPathSelector :: Selector '[] (Id NSString)
percentEncodedPathSelector = mkSelector "percentEncodedPath"

-- | @Selector@ for @setPercentEncodedPath:@
setPercentEncodedPathSelector :: Selector '[Id NSString] ()
setPercentEncodedPathSelector = mkSelector "setPercentEncodedPath:"

-- | @Selector@ for @percentEncodedQuery@
percentEncodedQuerySelector :: Selector '[] (Id NSString)
percentEncodedQuerySelector = mkSelector "percentEncodedQuery"

-- | @Selector@ for @setPercentEncodedQuery:@
setPercentEncodedQuerySelector :: Selector '[Id NSString] ()
setPercentEncodedQuerySelector = mkSelector "setPercentEncodedQuery:"

-- | @Selector@ for @percentEncodedFragment@
percentEncodedFragmentSelector :: Selector '[] (Id NSString)
percentEncodedFragmentSelector = mkSelector "percentEncodedFragment"

-- | @Selector@ for @setPercentEncodedFragment:@
setPercentEncodedFragmentSelector :: Selector '[Id NSString] ()
setPercentEncodedFragmentSelector = mkSelector "setPercentEncodedFragment:"

-- | @Selector@ for @encodedHost@
encodedHostSelector :: Selector '[] (Id NSString)
encodedHostSelector = mkSelector "encodedHost"

-- | @Selector@ for @setEncodedHost:@
setEncodedHostSelector :: Selector '[Id NSString] ()
setEncodedHostSelector = mkSelector "setEncodedHost:"

-- | @Selector@ for @rangeOfScheme@
rangeOfSchemeSelector :: Selector '[] NSRange
rangeOfSchemeSelector = mkSelector "rangeOfScheme"

-- | @Selector@ for @rangeOfUser@
rangeOfUserSelector :: Selector '[] NSRange
rangeOfUserSelector = mkSelector "rangeOfUser"

-- | @Selector@ for @rangeOfPassword@
rangeOfPasswordSelector :: Selector '[] NSRange
rangeOfPasswordSelector = mkSelector "rangeOfPassword"

-- | @Selector@ for @rangeOfHost@
rangeOfHostSelector :: Selector '[] NSRange
rangeOfHostSelector = mkSelector "rangeOfHost"

-- | @Selector@ for @rangeOfPort@
rangeOfPortSelector :: Selector '[] NSRange
rangeOfPortSelector = mkSelector "rangeOfPort"

-- | @Selector@ for @rangeOfPath@
rangeOfPathSelector :: Selector '[] NSRange
rangeOfPathSelector = mkSelector "rangeOfPath"

-- | @Selector@ for @rangeOfQuery@
rangeOfQuerySelector :: Selector '[] NSRange
rangeOfQuerySelector = mkSelector "rangeOfQuery"

-- | @Selector@ for @rangeOfFragment@
rangeOfFragmentSelector :: Selector '[] NSRange
rangeOfFragmentSelector = mkSelector "rangeOfFragment"

-- | @Selector@ for @queryItems@
queryItemsSelector :: Selector '[] (Id NSArray)
queryItemsSelector = mkSelector "queryItems"

-- | @Selector@ for @setQueryItems:@
setQueryItemsSelector :: Selector '[Id NSArray] ()
setQueryItemsSelector = mkSelector "setQueryItems:"

-- | @Selector@ for @percentEncodedQueryItems@
percentEncodedQueryItemsSelector :: Selector '[] (Id NSArray)
percentEncodedQueryItemsSelector = mkSelector "percentEncodedQueryItems"

-- | @Selector@ for @setPercentEncodedQueryItems:@
setPercentEncodedQueryItemsSelector :: Selector '[Id NSArray] ()
setPercentEncodedQueryItemsSelector = mkSelector "setPercentEncodedQueryItems:"

