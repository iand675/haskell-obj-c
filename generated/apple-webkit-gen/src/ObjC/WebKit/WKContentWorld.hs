{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKContentWorld object allows you to separate your application's interaction with content displayed in a WKWebView into different roles that cannot interfere with one another.
--
-- WKContentWorld objects should be treated as namespaces. This is useful for keeping your application's web content environment separate from the environment of the web page content itself,as well as managing multiple different environments within your own application.For example:- If you have complex scripting logic to bridge your web content to your application but your web content also has complex scripting libraries of its own,  you avoid possible conflicts by using a client WKContentWorld.- If you are writing a general purpose web browser that supports JavaScript extensions, you would use a different client WKContentWorld for each extension.
--
-- Since a WKContentWorld object is a namespace it does not contain any data itself.For example:- If you store a variable in JavaScript in the scope of a particular WKContentWorld while viewing a particular web page document, after navigating to a new document that variable will be gone.- If you store a variable in JavaScript in the scope of a particular WKContentWorld in one WKWebView, that variable will not exist in the same world in another WKWebView.
--
-- Generated bindings for @WKContentWorld@.
module ObjC.WebKit.WKContentWorld
  ( WKContentWorld
  , IsWKContentWorld(..)
  , new
  , init_
  , worldWithName
  , pageWorld
  , defaultClientWorld
  , name
  , defaultClientWorldSelector
  , initSelector
  , nameSelector
  , newSelector
  , pageWorldSelector
  , worldWithNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKContentWorld)
new  =
  do
    cls' <- getRequiredClass "WKContentWorld"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKContentWorld wkContentWorld => wkContentWorld -> IO (Id WKContentWorld)
init_ wkContentWorld =
  sendOwnedMessage wkContentWorld initSelector

-- | Retrieves a named content world for API client use.
--
-- @name@ — The name of the WKContentWorld to retrieve.
--
-- When using a content world different from the page content world you can still manipulate the DOM and built-in DOM APIs but without conflicting with other aspects of the page content (e.g. JavaScript from the web page content itself)As long as a particular named WKContentWorld instance has not been deallocated, repeated calls with the same name will retrieve that same WKContentWorld instance.Each named content world is distinct from all other named content worlds, the defaultClientWorld, and the pageWorld.The name can be used to keep distinct worlds identifiable anywhere a world might be surfaced in a user interface. For example, the different worlds used in your application will be surfaced by name in the WebKit Web Inspector.
--
-- ObjC selector: @+ worldWithName:@
worldWithName :: IsNSString name => name -> IO (Id WKContentWorld)
worldWithName name =
  do
    cls' <- getRequiredClass "WKContentWorld"
    sendClassMessage cls' worldWithNameSelector (toNSString name)

-- | Retrieve the main world that page content itself uses.
--
-- When interacting with page content in a WKWebView using the page content world you can disrupt the operation of page content (e.g. by conflicting with variable names in JavaScript set by the web page content itself).
--
-- ObjC selector: @+ pageWorld@
pageWorld :: IO (Id WKContentWorld)
pageWorld  =
  do
    cls' <- getRequiredClass "WKContentWorld"
    sendClassMessage cls' pageWorldSelector

-- | Retrieve the default world for API client use.
--
-- When using a content world different from the page content world you can still manipulate the DOM and built-in DOM APIs but without conflicting with other aspects of the page content (e.g. JavaScript from the web page content itself)Repeated calls will retrieve the same WKContentWorld instance.
--
-- ObjC selector: @+ defaultClientWorld@
defaultClientWorld :: IO (Id WKContentWorld)
defaultClientWorld  =
  do
    cls' <- getRequiredClass "WKContentWorld"
    sendClassMessage cls' defaultClientWorldSelector

-- | The name of the WKContentWorld
--
-- The pageWorld and defaultClientWorld instances will have a nil name.All other instances will have the non-nil name they were accessed by.
--
-- ObjC selector: @- name@
name :: IsWKContentWorld wkContentWorld => wkContentWorld -> IO (Id NSString)
name wkContentWorld =
  sendMessage wkContentWorld nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKContentWorld)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKContentWorld)
initSelector = mkSelector "init"

-- | @Selector@ for @worldWithName:@
worldWithNameSelector :: Selector '[Id NSString] (Id WKContentWorld)
worldWithNameSelector = mkSelector "worldWithName:"

-- | @Selector@ for @pageWorld@
pageWorldSelector :: Selector '[] (Id WKContentWorld)
pageWorldSelector = mkSelector "pageWorld"

-- | @Selector@ for @defaultClientWorld@
defaultClientWorldSelector :: Selector '[] (Id WKContentWorld)
defaultClientWorldSelector = mkSelector "defaultClientWorld"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

