module Services where

singularity = "https://api.testeveonline.com"
tranquility = "https://api.eveonline.com"

char = "/char"
corp = "/corp"

-- API list
api = "/api/CallList.xml.aspx"

-- Account reletad
--needs API key
accStatus = "/account/AccountStatus.xml.aspx"
charList = "/account/Characters.xml.aspx"
keyInfo = "/account/APIKeyInfo.xml.aspx"

-- Character related
-- All below needs API key + characterID
charSheet = char ++ "/CharacterSheet.xml.aspx"
events = char ++ "/UpcomingCalendarEvents.xml.aspx"
eventAtten = char ++ "/CalendarEventAttendees.xml.aspx"
--needs ids (comma separeted)
mails     = char ++ "/MailBodies.xml.aspx"
mailMess  = char ++ "/MailMessages.xml.aspx"
mailLists = char ++ "/mailinglists.xml.aspx"
notifcontact = char ++ "/ContactNotifications.xml.aspx"
notif        = char ++ "/Notifications.xml.aspx"
--needs IDs (comma separeted)
notifTexts   = char ++ "/NotificationTexts.xml.aspx"
research = char ++ "/Research.xml.aspx"
skillTrain = char ++ "/SkillInTraining.xml.aspx"
skillQue = char ++ "/SkillQueue.xml.aspx"

-- Character/Corp related
-- All below needs API key + characterID
assets c = c ++ "/AssetList.xml.aspx"
contacts c = c ++ "/ContactList.xml.aspx"
killLog c = c ++ "/KillLog.xml.aspx"
--optional beforeKillID
facWar c = c ++ "/FacWarStats.xml.aspx"
industry c = c ++ "/IndustryJobs.xml.aspx"
--optional contractID
contracts c = c ++ "/char/Contracts.xml.aspx"
contractBids c = c ++ "/ContractBids.xml.aspx"
--needs contractID
contractItems c = c ++ "/char/ContractItems.xml.aspx"
--opional orderID
marketOrds c = c ++ "/MarketOrders.xml.aspx"
medals c = c ++ "/Medals.xml.aspx"
--needs IDs (comma separeted)
itemLoc c = c ++ "/Locations.xml.aspx"
--needseventIDs (comma separeted)
standing c = c ++ "/Standings.xml.aspx"
accBalance c = c ++ "/AccountBalance.xml.aspx"
--needs accountKey, optional fromID, optional rowCount
accJour c = c ++ "/WalletJournal.xml.aspx"
--needs accountKey, optional fromID, optional rowCount
accTrans c = c ++ "/WalletTransactions.xml.aspx"

-- Corp Related
-- Needs Corp API
corpSheet = corp ++ "CorporationSheet.xml.aspx"
-- All below need Needs API key + characterID
containerLog = corp ++ "/ContainerLog.xml.aspx"
-- No characterID, optional extended
members      = corp ++ "/MemberTracking.xml.aspx"
memberSec    = corp ++ "/MemberSecurity.xml.aspx"
memberSecLog = corp ++ "/MemberSecurityLog.xml.aspx"
memberMedals = corp ++ "/MemberMedals.xml.aspx"
-- No characterID
poses     = corp ++ "/StarbaseList.xml.aspx"
-- Needs itemID, No characterID
posDetail = corp ++ "/corp/StarbaseDetail.xml.aspx"
shareHolders = corp ++ "/Shareholders.xml.aspx"
stations       = corp ++ "/OutpostList.xml.aspx"
-- Needs itemID
stationService = corp ++"/OutpostServiceDetail.xml.aspx"
titles = corp ++ "/Titles.xml.aspx"


-------- Test data
charID :: (String,String)
charID = ("characterID", "94792304")

--Character key
testKey1 :: [(String,String)]
testKey1 =
  [("keyID","3523746")
  ,("vCode","f3hP9AowjgJXd9j4FuNSz8A5PGUlciG6a7t1vQvOj4jVt8TZsEFWjoSbhKxrFJCl")]

--Account Key
testKey2 :: [(String,String)]
testKey2 =
  [("keyID","3523745")
  ,("vCode","ZGNUWzSDUmoMsTiVrcM4vMQxDkzK8xD4iVTySEiLr9xKEqn3xe3cvalCXmZpdL71")]