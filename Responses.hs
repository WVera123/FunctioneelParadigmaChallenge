module Responses where

-- General
msgWelcome         = "Welkom bij PostNL! Hoe kan ik u helpen? (Typ 'help' voor opties of 'exit' om te stoppen)"
msgGreeting        = "Hallo! Ik ben de PostNL chatbot. Waarmee kan ik u van dienst zijn?"
msgExit            = "Bedankt voor het gebruiken van PostNL. Tot ziens!"
msgUnknown         = "Ik begrijp het niet helemaal."

-- Tracking Flow
msgAskTracking     = "Om uw pakketje te volgen heb ik uw 6-cijferige trackingnummer nodig. Typ 'help' voor meer info."
msgAskPostalCode   = "Om te controleren waar uw pakket is heb ik een aantal gegevens nodig. Wat is de postcode van de ontvanger?"
msgAskHouseNumber  = "Postcode ontvangen. Wat is uw huisnummer?"
msgWaiting         = "Bedankt! De gegevens kloppen. We halen de gegevens van uw pakket op, even geduld..."
msgFinalDelay      = "Uw pakket is onderweg, maar er is een kleine vertraging. We verwachten dat het binnen 2 dagen bij u is."
msgFinalProblem    = "Er lijkt een probleem te zijn met uw pakket. Neem contact op met onze klantenservice voor meer informatie."
msgFinalSuccess    = "Goed nieuws! Uw pakket is onderweg en wordt binnen 1 werkdag bij u afgeleverd."

-- Error Handling
msgInvalidPostal   = "Dat lijkt geen geldige postcode. Gebruik het formaat '1234 AB'."
msgInvalidHouse    = "Dat lijkt geen geldig huisnummer. Probeer het opnieuw."

-- Contextual Help
helpIdle           = "U kunt vragen: 'Waar is mijn pakket?'."
helpTracking       = "Uw trackingnummer kunt u vinden in de bevestigingsmail van uw bestelling en bestaat uit 6 cijfers."
helpPostal         = "Voer de 4 cijfers en 2 letters van uw postcode in."
helpHouse          = "Voer uw huisnummer en eventuele toevoeging in."
