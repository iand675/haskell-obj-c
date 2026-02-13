{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeICEServerStruct@.
module ObjC.Matter.MTRDataTypeICEServerStruct
  ( MTRDataTypeICEServerStruct
  , IsMTRDataTypeICEServerStruct(..)
  , urls
  , setUrls
  , username
  , setUsername
  , credential
  , setCredential
  , caid
  , setCaid
  , caidSelector
  , credentialSelector
  , setCaidSelector
  , setCredentialSelector
  , setUrlsSelector
  , setUsernameSelector
  , urlsSelector
  , usernameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- urls@
urls :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSArray)
urls mtrDataTypeICEServerStruct =
  sendMessage mtrDataTypeICEServerStruct urlsSelector

-- | @- setUrls:@
setUrls :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSArray value) => mtrDataTypeICEServerStruct -> value -> IO ()
setUrls mtrDataTypeICEServerStruct value =
  sendMessage mtrDataTypeICEServerStruct setUrlsSelector (toNSArray value)

-- | @- username@
username :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSString)
username mtrDataTypeICEServerStruct =
  sendMessage mtrDataTypeICEServerStruct usernameSelector

-- | @- setUsername:@
setUsername :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSString value) => mtrDataTypeICEServerStruct -> value -> IO ()
setUsername mtrDataTypeICEServerStruct value =
  sendMessage mtrDataTypeICEServerStruct setUsernameSelector (toNSString value)

-- | @- credential@
credential :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSString)
credential mtrDataTypeICEServerStruct =
  sendMessage mtrDataTypeICEServerStruct credentialSelector

-- | @- setCredential:@
setCredential :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSString value) => mtrDataTypeICEServerStruct -> value -> IO ()
setCredential mtrDataTypeICEServerStruct value =
  sendMessage mtrDataTypeICEServerStruct setCredentialSelector (toNSString value)

-- | @- caid@
caid :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSNumber)
caid mtrDataTypeICEServerStruct =
  sendMessage mtrDataTypeICEServerStruct caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSNumber value) => mtrDataTypeICEServerStruct -> value -> IO ()
setCaid mtrDataTypeICEServerStruct value =
  sendMessage mtrDataTypeICEServerStruct setCaidSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @urls@
urlsSelector :: Selector '[] (Id NSArray)
urlsSelector = mkSelector "urls"

-- | @Selector@ for @setUrls:@
setUrlsSelector :: Selector '[Id NSArray] ()
setUrlsSelector = mkSelector "setUrls:"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector '[Id NSString] ()
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @credential@
credentialSelector :: Selector '[] (Id NSString)
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector '[Id NSString] ()
setCredentialSelector = mkSelector "setCredential:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

