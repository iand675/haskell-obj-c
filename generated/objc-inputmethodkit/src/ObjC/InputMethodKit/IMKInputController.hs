{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IMKInputController
--
-- The basic class that controls input on the input method side.
--
-- IMKInputController implements fully implements the protocols defined above.  Typically a developer does not override this class, but provides a delegate object that implements the methods that developer is interested in.  The IMKInputController versions of the protocol methods check if the delegate object implements a method, and  call the delegate version if it exists.
--
-- The IMKServer class which is allocated in an input method's main function creates a controller class for each input session created by a client application. Therefore for every input session there is a corresponding IMKInputController.
--
-- Generated bindings for @IMKInputController@.
module ObjC.InputMethodKit.IMKInputController
  ( IMKInputController
  , IsIMKInputController(..)
  , initWithServer_delegate_client
  , updateComposition
  , cancelComposition
  , compositionAttributesAtRange
  , selectionRange
  , replacementRange
  , markForStyle_atRange
  , doCommandBySelector_commandDictionary
  , hidePalettes
  , menu
  , delegate
  , setDelegate
  , server
  , client
  , inputControllerWillClose
  , annotationSelected_forCandidate
  , candidateSelectionChanged
  , candidateSelected
  , initWithServer_delegate_clientSelector
  , updateCompositionSelector
  , cancelCompositionSelector
  , compositionAttributesAtRangeSelector
  , selectionRangeSelector
  , replacementRangeSelector
  , markForStyle_atRangeSelector
  , doCommandBySelector_commandDictionarySelector
  , hidePalettesSelector
  , menuSelector
  , delegateSelector
  , setDelegateSelector
  , serverSelector
  , clientSelector
  , inputControllerWillCloseSelector
  , annotationSelected_forCandidateSelector
  , candidateSelectionChangedSelector
  , candidateSelectedSelector


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

import ObjC.InputMethodKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes the controller class setting the delegate.
--
-- The inputClient parameter is the client side object that will be sending messages to the controller via the IMKServer.  The client object always conforms to the IMKTextInput protocol.
--
-- Methods in the protocols that are implemented by the delegate object always include a client parameter.  Methods in the IMKInputController class do not take a client.  This is because the client is stored as an ivar in the IMKInputController.
--
-- ObjC selector: @- initWithServer:delegate:client:@
initWithServer_delegate_client :: (IsIMKInputController imkInputController, IsIMKServer server) => imkInputController -> server -> RawId -> RawId -> IO RawId
initWithServer_delegate_client imkInputController  server delegate inputClient =
withObjCPtr server $ \raw_server ->
    fmap (RawId . castPtr) $ sendMsg imkInputController (mkSelector "initWithServer:delegate:client:") (retPtr retVoid) [argPtr (castPtr raw_server :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr (unRawId inputClient) :: Ptr ())]

-- | Called to inform the controller that the composition has changed.
--
-- This method will call the protocol method composedString: to obtain the current composition. The current composition will be sent to the client by a call to the method setMarkedText:
--
-- ObjC selector: @- updateComposition@
updateComposition :: IsIMKInputController imkInputController => imkInputController -> IO ()
updateComposition imkInputController  =
  sendMsg imkInputController (mkSelector "updateComposition") retVoid []

-- | Stops the current composition and replaces marked text with the original text.
--
-- Calls the method originalString to obtain the original text and sends that to the client via a call to IMKInputSession protocol method insertText:
--
-- ObjC selector: @- cancelComposition@
cancelComposition :: IsIMKInputController imkInputController => imkInputController -> IO ()
cancelComposition imkInputController  =
  sendMsg imkInputController (mkSelector "cancelComposition") retVoid []

-- | Called to obtain a dictionary of text attributes.
--
-- The default implementation returns an empty dictionary.  You should override this method if your input method wants to provide font or glyphInformation. The returned object should be an autoreleased object.
--
-- ObjC selector: @- compositionAttributesAtRange:@
compositionAttributesAtRange :: IsIMKInputController imkInputController => imkInputController -> NSRange -> IO (Id NSMutableDictionary)
compositionAttributesAtRange imkInputController  range =
  sendMsg imkInputController (mkSelector "compositionAttributesAtRange:") (retPtr retVoid) [argNSRange range] >>= retainedObject . castPtr

-- | Returns where the selection should be placed inside markedText.
--
-- This method is called by updateComposition: to obtain the selection range for markedText.  The default implementation sets the selection range at the end of the marked text.
--
-- ObjC selector: @- selectionRange@
selectionRange :: IsIMKInputController imkInputController => imkInputController -> IO NSRange
selectionRange imkInputController  =
  sendMsgStret imkInputController (mkSelector "selectionRange") retNSRange []

-- | Returns the range in the client document that text should replace.
--
-- This method is called by updateComposition to obtain the range in the client's document where markedText should be placed.  The default implementation returns an NSRange whose location and length are NSNotFound.  That indicates that the marked text should be placed at the current insertion point.  Input methods that wish to insert marked text somewhere other than at the current insertion point should override this method.
--
-- An example of an input method that might override this method would be one replaced words with synonyms.  That input method would watch for certain words and when one of those words was seen it would be replaced by marked text that was a synonym of the word.
--
-- ObjC selector: @- replacementRange@
replacementRange :: IsIMKInputController imkInputController => imkInputController -> IO NSRange
replacementRange imkInputController  =
  sendMsgStret imkInputController (mkSelector "replacementRange") retNSRange []

-- | Returns a dictionary of text attributes that can be used to mark a range of an attributed string that is going to be sent to a client.
--
-- This utility function can be called by input methods to mark each range (i.e. clause ) of marked text.  The style parameter should be one of the following values: kTSMHiliteSelectedRawText, kTSMHiliteConvertedText or kTSMHiliteSelectedConvertedText. See AERegistry.h for the definition of these values.
--
-- The default implementation begins by calling compositionAttributesAtRange: to obtain extra attributes that an input method wants to include such as font or  glyph information.  Then the appropriate underline and underline color information is added to the attributes dictionary for the style parameter.
--
-- Finally the style value is added as dictionary value.  The key for the style value is NSMarkedClauseSegment. The returned object should be an autoreleased object.
--
-- ObjC selector: @- markForStyle:atRange:@
markForStyle_atRange :: IsIMKInputController imkInputController => imkInputController -> CLong -> NSRange -> IO (Id NSDictionary)
markForStyle_atRange imkInputController  style range =
  sendMsg imkInputController (mkSelector "markForStyle:atRange:") (retPtr retVoid) [argCLong (fromIntegral style), argNSRange range] >>= retainedObject . castPtr

-- | Called to pass commands that are not generated as part of the text input.
--
-- The default implementation checks if the controller object (i.e. self) responds to the selector.  If that is true the message performSelector:withObject  is sent to the controller class.  The object parameter in that case is the infoDictionary.
--
-- This method is called when a user selects a command item from the text input menu.  To support this an input method merely needs to provide actions for each menu item that will be placed in the menu.
--
-- i.e. -(void)menuAction:(id)sender
--
-- Note that the sender in this instance will be the infoDictionary.  The dictionary contains two values:
--
-- kIMKCommandMenuItemName			NSMenuItem  -- the NSMenuItem that was selected  	kIMKCommandClientName			id<IMKTextInput, NSObject> - the current client
--
-- ObjC selector: @- doCommandBySelector:commandDictionary:@
doCommandBySelector_commandDictionary :: (IsIMKInputController imkInputController, IsNSDictionary infoDictionary) => imkInputController -> Selector -> infoDictionary -> IO ()
doCommandBySelector_commandDictionary imkInputController  aSelector infoDictionary =
withObjCPtr infoDictionary $ \raw_infoDictionary ->
    sendMsg imkInputController (mkSelector "doCommandBySelector:commandDictionary:") retVoid [argPtr (unSelector aSelector), argPtr (castPtr raw_infoDictionary :: Ptr ())]

-- | Called to inform an input method that any visible UI should be closed.
--
-- ObjC selector: @- hidePalettes@
hidePalettes :: IsIMKInputController imkInputController => imkInputController -> IO ()
hidePalettes imkInputController  =
  sendMsg imkInputController (mkSelector "hidePalettes") retVoid []

-- | Returns a menu of input method specific commands.
--
-- This method is called whenever the menu needs to be drawn so that input methods can update the menu to reflect their current state. The returned NSMenu is an autoreleased object.
--
-- ObjC selector: @- menu@
menu :: IsIMKInputController imkInputController => imkInputController -> IO (Id NSMenu)
menu imkInputController  =
  sendMsg imkInputController (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | - (id)delegate;
--
-- Returns the input controller's delegate object. The returned id is an autoreleased object.
--
-- ObjC selector: @- delegate@
delegate :: IsIMKInputController imkInputController => imkInputController -> IO RawId
delegate imkInputController  =
  fmap (RawId . castPtr) $ sendMsg imkInputController (mkSelector "delegate") (retPtr retVoid) []

-- | Set the input controller's delegate object.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIMKInputController imkInputController => imkInputController -> RawId -> IO ()
setDelegate imkInputController  newDelegate =
  sendMsg imkInputController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId newDelegate) :: Ptr ())]

-- | Return the server object which is managing this input controller. The returned IMKServer is an autoreleased object.
--
-- ObjC selector: @- server@
server :: IsIMKInputController imkInputController => imkInputController -> IO (Id IMKServer)
server imkInputController  =
  sendMsg imkInputController (mkSelector "server") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns this controller's client object.
--
-- The client object will conform to the IMKTextInput protocol. The returned object is an autoreleased object.
--
-- ObjC selector: @- client@
client :: IsIMKInputController imkInputController => imkInputController -> IO RawId
client imkInputController  =
  fmap (RawId . castPtr) $ sendMsg imkInputController (mkSelector "client") (retPtr retVoid) []

-- | Called to notify an input controller that it is about to be closed.
--
-- ObjC selector: @- inputControllerWillClose@
inputControllerWillClose :: IsIMKInputController imkInputController => imkInputController -> IO ()
inputControllerWillClose imkInputController  =
  sendMsg imkInputController (mkSelector "inputControllerWillClose") retVoid []

-- | Called when a user has selected a annotation in a candidate window.
--
-- When a candidate window is displayed and the user selects an annotation the selected annotation is sent to the input controller via this method.  The currently selected candidateString is also sent to the input method.
--
-- ObjC selector: @- annotationSelected:forCandidate:@
annotationSelected_forCandidate :: (IsIMKInputController imkInputController, IsNSAttributedString annotationString, IsNSAttributedString candidateString) => imkInputController -> annotationString -> candidateString -> IO ()
annotationSelected_forCandidate imkInputController  annotationString candidateString =
withObjCPtr annotationString $ \raw_annotationString ->
  withObjCPtr candidateString $ \raw_candidateString ->
      sendMsg imkInputController (mkSelector "annotationSelected:forCandidate:") retVoid [argPtr (castPtr raw_annotationString :: Ptr ()), argPtr (castPtr raw_candidateString :: Ptr ())]

-- | Informs an input controller that the current candidate selection in the candidate window has changed.
--
-- The candidate parameter is the candidate string that the selection changed to.  Note this method is called to indicate that the user is moving around in the candidate window.  The candidate object is not a final selection.
--
-- ObjC selector: @- candidateSelectionChanged:@
candidateSelectionChanged :: (IsIMKInputController imkInputController, IsNSAttributedString candidateString) => imkInputController -> candidateString -> IO ()
candidateSelectionChanged imkInputController  candidateString =
withObjCPtr candidateString $ \raw_candidateString ->
    sendMsg imkInputController (mkSelector "candidateSelectionChanged:") retVoid [argPtr (castPtr raw_candidateString :: Ptr ())]

-- | Called when a new candidate has been finally selected.
--
-- The candidate parameter is the users final choice from the candidate window. The candidate window will have been closed before this method is called.
--
-- ObjC selector: @- candidateSelected:@
candidateSelected :: (IsIMKInputController imkInputController, IsNSAttributedString candidateString) => imkInputController -> candidateString -> IO ()
candidateSelected imkInputController  candidateString =
withObjCPtr candidateString $ \raw_candidateString ->
    sendMsg imkInputController (mkSelector "candidateSelected:") retVoid [argPtr (castPtr raw_candidateString :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServer:delegate:client:@
initWithServer_delegate_clientSelector :: Selector
initWithServer_delegate_clientSelector = mkSelector "initWithServer:delegate:client:"

-- | @Selector@ for @updateComposition@
updateCompositionSelector :: Selector
updateCompositionSelector = mkSelector "updateComposition"

-- | @Selector@ for @cancelComposition@
cancelCompositionSelector :: Selector
cancelCompositionSelector = mkSelector "cancelComposition"

-- | @Selector@ for @compositionAttributesAtRange:@
compositionAttributesAtRangeSelector :: Selector
compositionAttributesAtRangeSelector = mkSelector "compositionAttributesAtRange:"

-- | @Selector@ for @selectionRange@
selectionRangeSelector :: Selector
selectionRangeSelector = mkSelector "selectionRange"

-- | @Selector@ for @replacementRange@
replacementRangeSelector :: Selector
replacementRangeSelector = mkSelector "replacementRange"

-- | @Selector@ for @markForStyle:atRange:@
markForStyle_atRangeSelector :: Selector
markForStyle_atRangeSelector = mkSelector "markForStyle:atRange:"

-- | @Selector@ for @doCommandBySelector:commandDictionary:@
doCommandBySelector_commandDictionarySelector :: Selector
doCommandBySelector_commandDictionarySelector = mkSelector "doCommandBySelector:commandDictionary:"

-- | @Selector@ for @hidePalettes@
hidePalettesSelector :: Selector
hidePalettesSelector = mkSelector "hidePalettes"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @server@
serverSelector :: Selector
serverSelector = mkSelector "server"

-- | @Selector@ for @client@
clientSelector :: Selector
clientSelector = mkSelector "client"

-- | @Selector@ for @inputControllerWillClose@
inputControllerWillCloseSelector :: Selector
inputControllerWillCloseSelector = mkSelector "inputControllerWillClose"

-- | @Selector@ for @annotationSelected:forCandidate:@
annotationSelected_forCandidateSelector :: Selector
annotationSelected_forCandidateSelector = mkSelector "annotationSelected:forCandidate:"

-- | @Selector@ for @candidateSelectionChanged:@
candidateSelectionChangedSelector :: Selector
candidateSelectionChangedSelector = mkSelector "candidateSelectionChanged:"

-- | @Selector@ for @candidateSelected:@
candidateSelectedSelector :: Selector
candidateSelectedSelector = mkSelector "candidateSelected:"

