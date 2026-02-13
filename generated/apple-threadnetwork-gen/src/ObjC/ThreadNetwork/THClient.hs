{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that supports safely sharing Thread credentials between multiple clients.
--
-- Request credentials for either a specific Thread network or for the _preferred network_ using @THClient@. The preferred network is the default Thread network chosen by the framework for a home.
--
-- The ThreadNetwork framework maintains a database of network credentials. The class allows clients to store, list, and delete credentials for a given network from the database.
--
-- Some methods in @THClient@ use the _team ID_, a string that you store in your application’s @Info.plist@. The ThreadNetwork framework uses the team ID to preserve the privacy of the Thread network credentials across different clients. For example, credentials stored by one client can’t be deleted or modified by another client.
--
-- - Important: Thread credentials give you the ability to add any device into   the Thread network. Use this information responsibly.
--
-- Generated bindings for @THClient@.
module ObjC.ThreadNetwork.THClient
  ( THClient
  , IsTHClient(..)
  , init_
  , deleteCredentialsForBorderAgent_completion
  , retrieveCredentialsForBorderAgent_completion
  , storeCredentialsForBorderAgent_activeOperationalDataSet_completion
  , retrievePreferredCredentials
  , retrieveCredentialsForExtendedPANID_completion
  , checkPreferredNetworkForActiveOperationalDataset_completion
  , isPreferredNetworkAvailableWithCompletion
  , checkPreferredNetworkForActiveOperationalDataset_completionSelector
  , deleteCredentialsForBorderAgent_completionSelector
  , initSelector
  , isPreferredNetworkAvailableWithCompletionSelector
  , retrieveCredentialsForBorderAgent_completionSelector
  , retrieveCredentialsForExtendedPANID_completionSelector
  , retrievePreferredCredentialsSelector
  , storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ThreadNetwork.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates the client object.
--
-- - Returns: An instance of the client object.
--
-- ObjC selector: @- init@
init_ :: IsTHClient thClient => thClient -> IO (Id THClient)
init_ thClient =
  sendOwnedMessage thClient initSelector

-- | Deletes Thread network credentials from the framework database for a Border Agent.
--
-- The Border Agent is the software component running in the Border Router responsible for advertising itself in the Wi-Fi or Ethernet network.
--
-- - Parameters:   - borderAgentID: The identifer of a Thread network Border Agent.   - completion: The completion handler the framework calls after deleting     the credentials.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func deleteCredentials(forBorderAgent borderAgentID: Data) async throws > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- deleteCredentialsForBorderAgent:completion:@
deleteCredentialsForBorderAgent_completion :: (IsTHClient thClient, IsNSData borderAgentID) => thClient -> borderAgentID -> Ptr () -> IO ()
deleteCredentialsForBorderAgent_completion thClient borderAgentID completion =
  sendMessage thClient deleteCredentialsForBorderAgent_completionSelector (toNSData borderAgentID) completion

-- | Requests Thread credentials for a Border Agent.
--
-- The framework identifies the developer by the team ID. When calling this method, you receive credentials for your team ID only.
--
-- - Parameters:   - borderAgentID: The identifer of a Thread network Border Agent.   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func credentials(forBorderAgentID borderAgentID: Data) async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrieveCredentialsForBorderAgent:completion:@
retrieveCredentialsForBorderAgent_completion :: (IsTHClient thClient, IsNSData borderAgentID) => thClient -> borderAgentID -> Ptr () -> IO ()
retrieveCredentialsForBorderAgent_completion thClient borderAgentID completion =
  sendMessage thClient retrieveCredentialsForBorderAgent_completionSelector (toNSData borderAgentID) completion

-- | Stores Thread network credentials into the framework database that a Border Agent provides.
--
-- The Border Agent is the software component running in the Border Router responsible for advertising itself in the Wi-Fi or Ethernet network.
--
-- The framework only stores credentials if it can find an mDNS record for the Border Agent that contains the specified Border Agent identifier.
--
-- - Parameters:   - borderAgentID: The identifer of an active Thread network Border Agent.   - activeOperationalDataSet: The essential operational parameters for the     Thread network.   - completion: The completion handler the framework calls after storing the credentials.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func storeCredentials(forBorderAgent borderAgentID: Data, activeOperationalDataSet: Data) async throws > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- storeCredentialsForBorderAgent:activeOperationalDataSet:completion:@
storeCredentialsForBorderAgent_activeOperationalDataSet_completion :: (IsTHClient thClient, IsNSData borderAgentID, IsNSData activeOperationalDataSet) => thClient -> borderAgentID -> activeOperationalDataSet -> Ptr () -> IO ()
storeCredentialsForBorderAgent_activeOperationalDataSet_completion thClient borderAgentID activeOperationalDataSet completion =
  sendMessage thClient storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector (toNSData borderAgentID) (toNSData activeOperationalDataSet) completion

-- | Requests Thread credentials for the preferred network.
--
-- When you call this method, an alert appears asking for user permission to access credentials.
--
-- - Parameters:   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func preferredCredentials() async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrievePreferredCredentials:@
retrievePreferredCredentials :: IsTHClient thClient => thClient -> Ptr () -> IO ()
retrievePreferredCredentials thClient completion =
  sendMessage thClient retrievePreferredCredentialsSelector completion

-- | Requests Thread credentials for an extended Personal Area Network (PAN) ID.
--
-- When calling this method, an alert appears asking for user permission to access credentials.
--
-- - Parameters:   - extendedPANID: The extended PAN identifier.   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func credentials(forExtendedPANID extendedPANID: Data) async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrieveCredentialsForExtendedPANID:completion:@
retrieveCredentialsForExtendedPANID_completion :: (IsTHClient thClient, IsNSData extendedPANID) => thClient -> extendedPANID -> Ptr () -> IO ()
retrieveCredentialsForExtendedPANID_completion thClient extendedPANID completion =
  sendMessage thClient retrieveCredentialsForExtendedPANID_completionSelector (toNSData extendedPANID) completion

-- | Determines if the essential operating parameters match the preferred network’s parameters.
--
-- - Parameters:   - activeOperationalDataSet: The essential operating parameters to compare     against the preferred network’s parameters.   - completion: The completion handler that returns the result of the     comparison.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func isPreferred(forActiveOperationalDataset activeOperationalDataSet: Data) async -> Bool > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- checkPreferredNetworkForActiveOperationalDataset:completion:@
checkPreferredNetworkForActiveOperationalDataset_completion :: (IsTHClient thClient, IsNSData activeOperationalDataSet) => thClient -> activeOperationalDataSet -> Ptr () -> IO ()
checkPreferredNetworkForActiveOperationalDataset_completion thClient activeOperationalDataSet completion =
  sendMessage thClient checkPreferredNetworkForActiveOperationalDataset_completionSelector (toNSData activeOperationalDataSet) completion

-- | Determines if the preferred network is available or not
--
-- - Parameters:   - completion: The completion handler that returns the result of the     preferred network availability.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func isPreferredAvailable() async -> Bool > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- isPreferredNetworkAvailableWithCompletion:@
isPreferredNetworkAvailableWithCompletion :: IsTHClient thClient => thClient -> Ptr () -> IO ()
isPreferredNetworkAvailableWithCompletion thClient completion =
  sendMessage thClient isPreferredNetworkAvailableWithCompletionSelector completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id THClient)
initSelector = mkSelector "init"

-- | @Selector@ for @deleteCredentialsForBorderAgent:completion:@
deleteCredentialsForBorderAgent_completionSelector :: Selector '[Id NSData, Ptr ()] ()
deleteCredentialsForBorderAgent_completionSelector = mkSelector "deleteCredentialsForBorderAgent:completion:"

-- | @Selector@ for @retrieveCredentialsForBorderAgent:completion:@
retrieveCredentialsForBorderAgent_completionSelector :: Selector '[Id NSData, Ptr ()] ()
retrieveCredentialsForBorderAgent_completionSelector = mkSelector "retrieveCredentialsForBorderAgent:completion:"

-- | @Selector@ for @storeCredentialsForBorderAgent:activeOperationalDataSet:completion:@
storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector :: Selector '[Id NSData, Id NSData, Ptr ()] ()
storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector = mkSelector "storeCredentialsForBorderAgent:activeOperationalDataSet:completion:"

-- | @Selector@ for @retrievePreferredCredentials:@
retrievePreferredCredentialsSelector :: Selector '[Ptr ()] ()
retrievePreferredCredentialsSelector = mkSelector "retrievePreferredCredentials:"

-- | @Selector@ for @retrieveCredentialsForExtendedPANID:completion:@
retrieveCredentialsForExtendedPANID_completionSelector :: Selector '[Id NSData, Ptr ()] ()
retrieveCredentialsForExtendedPANID_completionSelector = mkSelector "retrieveCredentialsForExtendedPANID:completion:"

-- | @Selector@ for @checkPreferredNetworkForActiveOperationalDataset:completion:@
checkPreferredNetworkForActiveOperationalDataset_completionSelector :: Selector '[Id NSData, Ptr ()] ()
checkPreferredNetworkForActiveOperationalDataset_completionSelector = mkSelector "checkPreferredNetworkForActiveOperationalDataset:completion:"

-- | @Selector@ for @isPreferredNetworkAvailableWithCompletion:@
isPreferredNetworkAvailableWithCompletionSelector :: Selector '[Ptr ()] ()
isPreferredNetworkAvailableWithCompletionSelector = mkSelector "isPreferredNetworkAvailableWithCompletion:"

