module Responses where

msgGreeting :: String
msgGreeting = "Hallo! Hoe kan ik u helpen?"

msgAskTracking :: String
msgAskTracking = "Om uw pakketje te volgen heb ik uw trackingnummer nodig."

msgRejectTracking :: String
msgRejectTracking = "U moet eerst vragen waar uw pakketje is voordat u een nummer opgeeft."

msgTrackingSuccess :: String
msgTrackingSuccess = "Bedankt! Uw pakketje is onderweg en zal binnen 2 werkdagen bij u zijn."

msgTrackingRetry :: String
msgTrackingRetry = "Ik heb een trackingnummer nodig om u te helpen (bijv. '123456')."

msgUnknown :: String
msgUnknown = "Het spijt me, dat begrijp ik niet. Bedoelt u 'waar is mijn pakketje'?"

msgExit :: String
msgExit = "Tot ziens!"

msgWelcome :: String
msgWelcome = "PostNL Chatbot gestart. Typ 'exit' om te stoppen."