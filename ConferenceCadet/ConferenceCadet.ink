// VARIABLES:
// ---------------------------------------------------

// Healths:
VAR energy = 0
  // starts at zero cos of the delayed flight
  // reset to 8 at the start of each day
  // each activity bar rest costs 1
VAR socialBattery = 3
  // starts at 3 everyday
  // social choices are conditional on this not being 0
  // each social choice decreases this
VAR drank = false
  
// Things earnt:
VAR sessionsAttended = 0
VAR academicsMet = 0
VAR friendsMade = 0
VAR funMemoriesMade = 0
VAR ideasInspired = 0
VAR dessertsTried = 0

// Goals achieved:
VAR metMeng = false
VAR connectedWithMeng = false
VAR impressedMeng = false
// The first goal will be evaluated in three levels: met, connected, and impressed
VAR bxSessionAttended = false
// second goal will be evaluated based on whether or not they attended the BX session

// other
VAR samExcursion = false

TODO nice to haves:
// * share to twitter button https://jsfiddle.net/tqbit/fordem3h/68/


// START OF STORY:
// ---------------------------------------------------
-> start
=== start ===
Welcome to ICFP24!

You are an introverted final year undergraduate student hoping to pursue a PhD in Programming Languages (PL). You are particularly interested in Bidirectional Programming (BX) and want to secure a studentship at the University of Bristol.

The main conference is three days long, with each day's schedule roughly following the pattern: keynote - break - session - lunch - session - break - session.
Tips for what to attend: keynotes, Student Research Competition (SRC), business meeting (including the announcement of ICFP25!), and reception are highly recommended. Other than that, take a look at the schedule and bookmark a couple of talks that sound interesting to you!

Your goals for the conference are:
1. Make a connection with Professor W, head of the Programming Languages Research Group at Bristol.
2. Get a feel for PL, and particularly BX research.

Ready?

* [Let's Go!]-> day1
* [I'd like to look at the program first] -> program

=== program ===
Follow this link to take a look at the program:
icfp24.sigplan.org/program/program-icfp-2024/

* [I'm now ready. Let's go!] -> day1

// DAY ONE:
// ---------------------------------------------------
=== day1 ===
DAY ONE

Your flight has been delayed. Instead of landing the night before the conference starts, you have landed at 6am the day the conference starts.

You are exhausted.

You have just managed to check into your hotel and could still make the keynote. What do you do?
* [Attend the keynote!] -> keynote
* [Take a nap] -> day1.nap
* [Down espresso and head to the keynote!] -> espresso

= nap
What would you like to wake up for?
* [The morning coffee break] Your nap wasn't long enough. -> exhaustion
* [The Session on Algebraic and Computational Effects] Your nap wasn't long enough. -> exhaustion
* [Lunch]
    ~ energy = energy + 4
    You sleep till lunch. You replenish your energy levels to {energy} / 9.
    -> lunch
* [The Session on Type Theory]
    ~ energy = energy + 5
    You sleep till the Types session. You replenish your energy levels to {energy} / 9.
    -> typesSession
* [The afternoon break]
    ~ energy = energy + 6
    You sleep till the afternoon break. You replenish your energy levels to {energy} / 9.
    -> afternoonBreak
* [The Session on Logical Foundations]
    ~ energy = energy + 7
    You sleep till the Logic session. You replenish your energy levels to {energy} / 9.
    -> logicSession
* [The Memorial for D. Turner & Arvind]
    ~ energy = energy + 8
    You sleep till the memorial. You replenish your energy levels to {energy} / 9.
    -> memorial
* [The Reception]
    ~ energy = energy + 8
    You sleep till the reception. You replenish your energy levels to {energy} / 9.
    -> reception

= espresso
You down three of Italy's famous espressos, hoping it will give you life. -> keynote
= keynote
You desperately try to pay attention to Andy's keynote, but you cannot. {espresso: The coffee is making your head spin.} -> exhaustion

// skipped sessions for nap, narrative never goes here
//= morningBreak
//morning break
//= effectsSession
//Session on Algebraic and Computational Effects

= lunch
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}

It's lunch!

How would you like to spend it?
// random change at networking with Professor W
~ temp meng = RANDOM(1,10)
* {meng == 5 && metMeng == false && socialBattery > 1} [You see Professor W! You bite the bullet to go and introduce yourself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   -> contest
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [You see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
  -> contest
* {socialBattery > 0} [Making friends with your peers]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You spend lunch making friends with your peers. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   -> contest
* {socialBattery > 0} [Networking with academics]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You spend lunch meeting academics. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> contest
* [Trying all the desserts]
   You spend lunch deciding which dessert you like the best. The desserts were all delicious, but the cheesecake took the biscuit.
   ~ funMemoriesMade = funMemoriesMade + 1
   ~ dessertsTried = dessertsTried + 1
   -> contest
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> contest

= contest
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
The ICFP Programming Contest results will be announced during the last 30mins of lunch. Will you go?
* [Of course!]
    You attend the results and enjoy hearing so much about the contest that you resolve to enter with your friends next year!
    ~ funMemoriesMade = funMemoriesMade + 1
    ~ sessionsAttended = sessionsAttended + 1
    ~ ideasInspired = ideasInspired + 1
  -> typesSession
* [Nah] You continue enjoying your lunch break.
  -> typesSession

= typesSession
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Type Theory
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> afternoonBreak
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> afternoonBreak
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> afternoonBreak
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> afternoonBreak

= afternoonBreak
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Afternoon Break
~ temp friendsNeeded = RANDOM(2,10)
// random change at networking with Professor W
~ temp meng = RANDOM(1,10)
* {meng == 5 && metMeng == false && socialBattery > 1} [I see Professor W! I'm going to bite the bullet to go and introduce myself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   -> logicSession
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [I see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
  -> logicSession
* [I'm going to drink some coffee]
  You drink some coffee, energising yourself.
  ~ energy = energy + 1
  -> logicSession
* {friendsMade > friendsNeeded} [I see some friends I made earlier. I'll chat to them]
  You chat to your friends.
  ~ energy = energy - 1
  -> logicSession
* {socialBattery > 0} [I'm going to network]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You use the break to network. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> logicSession
* {socialBattery > 0} [I'm going to try and make some friends]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You go introduce yourself to some fellow students. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   -> logicSession
* [I'm going to get some fresh air outside and stretch my legs]
  You have a lovely break outside in sunny Milan.
  -> logicSession
* {friendsMade > 3} [I'm going to ask my friend that I see over there to introduce me to the people they are chatting to] Your friend introduces you to the people they are with.
   ~ energy = energy - 1
   ~ temp acss = RANDOM(0,2)
   ~ temp friendss = RANDOM(1,2)
   {acss == 1: You meet 1 academic.}
   {acss > 1: You meet {acss} academics.}
   {friendss == 1: You make 1 new friend.}
   {friendss > 1: You make {friendss} new friends.}
   ~ friendsMade = friendsMade + friendss
   ~ academicsMet = academicsMet + acss
   -> logicSession

* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> logicSession

= logicSession
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Logical Foundations
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> memorial
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(1,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> memorial
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> memorial
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> memorial

= memorial
You attend the Memorial for D. Turner & Arvind.
-> reception

= reception
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
ICFP Reception

How would you like to spend the evening?
* [I'm pretty tried, so I'm just going to show my face, eat some food then go to sleep]
    You say hi to the people you know, grab some food and head off.
    -> endOfDay
* [I'm going to spend the evening with people from my institution and relax]
   You have a nice relaxing evening with people from your institution. 
    -> endOfDay
* {socialBattery > 0} [I'm going to network!]
  You spend your evening chatting to people.
   ~ temp acs = RANDOM(2,5)
   ~ temp friends = RANDOM(2,5)
   You meet {acs} academics and make {friends} friends!
   ~ academicsMet = academicsMet + acs 
   ~ friendsMade = friendsMade + friends
   -> endOfDay
* {socialBattery == 0 && academicsMet > 1} [I'm going to make the most of these drinks tokens! My friend gave me theirs too, gonna be a fun night!]
   Because your social battery is empty by this point of the day, you drink a little too much to compensate and end up losing some connections with academics that you have made. Go sleep it off.
   ~ academicsMet = academicsMet - 2
   ~ drank = true
   -> endOfDay
* { socialBattery > 0}  [I'm going to make the most of these drinks tokens! My friend gave me theirs too, gonna be a fun night!]
  ~ temp mates = RANDOM(4,9)
  ~ drank = true
  ~ friendsMade = friendsMade + mates
  ~ funMemoriesMade = funMemoriesMade + 2
  You have a wonderful evening, with your confidence buoyed by then nice Italian wine, you end up making {mates} friends! Hopefully you won't have a headache in the morning.
  -> endOfDay

= endOfDay
Day One complete!
  * [On to Day Two] -> day2

// DAY TWO:
// ---------------------------------------------------
=== day2 ===
// replenish healths
{ drank:
  ~ energy = 7
- else: 
  ~ energy = 8
}

~ socialBattery = 3
DAY TWO
{drank: Your head still hurts a little from the night before, you might have a little less energy today.}
->keynote

= keynote
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Keynote: Capabilities for Control
* [I'm going to sleep in and miss the keynote]
   You have a nice little lie in, regaining some energy.
   ~energy = energy+2
   -> morningBreak
* [Of course I'm going to attend the keynote!]
  You enjoy the keynote.
  ~sessionsAttended = sessionsAttended +1
  ~ energy = energy - 1
  -> morningBreak

= morningBreak
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Morning Break
~ temp friendsNeeded = RANDOM(2,10)
// random change at networking with Professor W
~ temp meng = RANDOM(1,5)
* {meng == 5 && metMeng == false && socialBattery > 1} [I see Professor W! I'm going to bite the bullet to go and introduce myself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   -> 1Session
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [I see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
  -> 1Session
* [I'm going to drink some coffee]
  You drink some coffee, energising you.
  ~ energy = energy + 1
  -> 1Session
* {friendsMade > friendsNeeded} [I see some friends I already made. I'll chat to them]
  You chat to your friends.
  ~ energy = energy - 1
  -> 1Session
* {socialBattery > 0} [I'm going to network]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You use the break to network. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> 1Session
* {socialBattery > 0} [I'm going to try and make some friends]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You go introduce yourself to some fellow students. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   -> 1Session
* [I'm going to get some fresh air outside and stretch my legs]
  You have a lovely break outside in sunny Milan.
  -> 1Session
* {friendsMade > 3} [I'm going to ask my friend that I see over there to introduce me to the people they are chatting to] Your friend introduces you to the people they are with.
   ~ energy = energy - 1
   ~ temp acss = RANDOM(0,2)
   ~ temp friendss = RANDOM(1,2)
   {acss == 1: You meet 1 academic.}
   {acss > 1: You meet {acss} academics.}
   {friendss == 1: You make 1 new friend.}
   {friendss > 1: You make {friendss} new friends.}
   ~ friendsMade = friendsMade + friendss
   ~ academicsMet = academicsMet + acss
   -> 1Session
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> 1Session

= 1Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Meta-Programming, Staging, Generic Programming, Partial Evaluation

While you are deciding what to do this session, you notice that Professor W is going to be available in the hallway. This could be a good chance to network with him {metMeng: again}.

Suddenly, a student volunteer rushes up to you looking stressed. She hurriedly explains that she is part of the AV team and they could do with an extra pair of hands this session.

{socialBattery > 1: What do you do? Do you take your golden opportunity to network with Professor W? Or do you help the volunteer? | Your social battery is too low to go network with Professor W, what do you do?}

* {socialBattery > 1} [I go network with Professor W, this could be one of my few chances to secure a studentship at Bristol.] You apologise to the student volunteer, saying that you're busy and go network with Professor W.
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
    { connectedWithMeng == false && metMeng:
     ~ connectedWithMeng = true
     You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
    }
    { metMeng == false:
     ~ metMeng = true
     You introduce yourself to him. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
    }
    -> lunch

* [Of course I help the volunteer!]
  You help the volunteer from the AV team.
  It turns out that she is Professor W's PhD student, and because you helped her out she offers to introduce you to him at lunch.
  
  You spend lunch with them both, making an excellent impression on Professor W! This is the best you could have hoped for. Well done! :D
  
  ~ metMeng = true
  ~ connectedWithMeng = true
  ~ impressedMeng = true
  ~ energy = energy - 1
  
  -> SRCpres
  
* [Neither, I'm going to go and pay attention to the session]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> lunch
* [Neither. This is too much. I'm going to sit quietly somewhere]
  You have a nice little break.
  -> lunch
* [Neither. This is too much. I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1 
  -> lunch

= lunch
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
It's lunch!

How would you like to spend it?
// random change at networking with Professor W
~ temp meng = RANDOM(1,5)
* {meng == 5 && metMeng == false && socialBattery > 1} [You see Professor W! You bite the bullet to go and introduce yourself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   -> SRCpres
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [You see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
  -> SRCpres
* {socialBattery > 0} [Making friends with your peers]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You spend lunch making friends with your peers. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   -> SRCpres
* {socialBattery > 0} [Networking with academics]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You spend lunch networking. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> SRCpres
* [Trying all the desserts]
   You spent lunch trying all the desserts. They are delicious. You particularly like the brownie.
   ~ funMemoriesMade = funMemoriesMade + 1
   ~ dessertsTried = dessertsTried + 1
   -> SRCpres
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> SRCpres

= SRCpres
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
The Student Research Competition (SRC) presentations are during the last 30mins of lunch. Will you go?
* [Of course!]
    You attend the SRC presentations and get inspired!
    ~ funMemoriesMade = funMemoriesMade + 1
    ~ sessionsAttended = sessionsAttended + 1
    ~ ideasInspired = ideasInspired + 1
  -> 2Session
* [Nah] You continue enjoying your lunch break and skip the SRC competition.
  -> 2Session

= 2Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Separation Logic
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> afternoonBreak
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> afternoonBreak
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> afternoonBreak
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> afternoonBreak

= afternoonBreak
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Afternoon Break
~ temp friendsNeeded = RANDOM(2,10)
// random change at networking with Professor W
~ temp meng = RANDOM(1,5)
* {meng == 5 && metMeng == false && socialBattery > 1} [I see Professor W! I'm going to bite the bullet to go and introduce myself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   -> 3Session
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [I see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
  -> 3Session
* [I'm going to drink some coffee]
  You drink some coffee, energising you.
  ~ energy = energy + 1
  -> 3Session
* {friendsMade > friendsNeeded} [I see some friends I already made. I'll chat to them]
  You chat to your friends.
  ~ energy = energy - 1
  -> 3Session
* {socialBattery > 0} [I'm going to network]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You use the break to network. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> 3Session
* {socialBattery > 0} [I'm going to try and make some friends]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You go introduce yourself to some fellow students. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   -> 3Session
* [I'm going to get some fresh air outside and stretch my legs]
  You have a lovely break outside in sunny Milan.
  -> 3Session
* {friendsMade > 3} [I'm going to ask my friend that I see over there to introduce me to the people they are chatting to] Your friend introduces you to the people they are with.
   ~ energy = energy - 1
   ~ temp acss = RANDOM(0,2)
   ~ temp friendss = RANDOM(1,2)
   {acss == 1: You meet 1 academic.}
   {acss > 1: You meet {acss} academics.}
   {friendss == 1: You make 1 new friend.}
   {friendss > 1: You make {friendss} new friends.}
   ~ friendsMade = friendsMade + friendss
   ~ academicsMet = academicsMet + acss
   -> 3Session
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> 3Session

= 3Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Verification and Cost Analysis
* [I'm going to go and pay attention]
  You attend the session. It turns out this session has two BX talks, one of which by a PhD student at the University of Bristol! You love every minute if it.
  ~ bxSessionAttended = true
  ~ sessionsAttended = sessionsAttended + 1
  ~ energy = energy - 1
  ~ ideasInspired = ideasInspired + 2
  -> busMeeting
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> busMeeting
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> busMeeting
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> busMeeting
  
= busMeeting
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Business Meeting
* [Wouldn't miss it for the world!]
  ~ energy = energy - 1
  ~ funMemoriesMade = funMemoriesMade + 1
  You attend the business meeting and find out where next ICFP will be!
  -> dinner
* [I'm going to call it a day]
  You head back to your hotel, ending what you hope was another successful day.
  -> endOfDay
* {socialBattery > 0} [I'm going to get some more networking in]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> dinner

= dinner
Dinner time!
* {energy < 1} You are exhausted from the day, so just grab some food and head to bed. Maybe tomorrow you should try to build breaks into your day.
   -> endOfDay
* {energy > 0 && socialBattery > 0} [Socialisation in the evening is the best way to make lasting connections. I'm going to network!] You spend the evening networking over dinner.
    ~ temp acs = RANDOM(3,4)
   You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> endOfDay
* {energy > 0 && socialBattery > 0 && friendsMade > 0} [I want so have some fun this evening! I grab some of my friends and we have dinner] You have a lovely evening with your friends.
    ~ funMemoriesMade = funMemoriesMade + 1
   -> endOfDay
* {energy > 0} [I'm going to have a chill evening with a take out and some netflix so that I can wind down from the big day and get a good sleep ready for tomorrow.] You have a nice chill evening.
    -> endOfDay

= endOfDay
Day Two complete!
  * [On to Day Three] -> day3


// DAY THREE:
// ---------------------------------------------------
=== day3 ===
DAY THREE
// replenish healths
~ energy = 8
~ socialBattery = 3
->keynote

= keynote
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Keynote: Refinement Types from Light to Deep Verification
* [I'm going to sleep in and miss the keynote]
   You have a nice little lie in, regaining some energy.
   ~energy = energy+2
   -> morningBreak
* [Of course I'm going to attend the keynote!]
  You enjoy the keynote.
  ~sessionsAttended = sessionsAttended +1
  ~ energy = energy - 1
  -> morningBreak
-> morningBreak

= morningBreak
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Morning Break
~ temp friendsNeeded = RANDOM(2,10)
// random change at networking with Professor W
~ temp meng = RANDOM(1,5)
* {meng == 5 && metMeng == false && socialBattery > 1} [I see Professor W! I'm going to bite the bullet to go and introduce myself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   
   { bxSessionAttended:
        You also mention that you attended his student's talk the day before, which impresses him.
        ~ impressedMeng = true
    }
   -> 1Session
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [I see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
   
   { bxSessionAttended:
        You also discuss his student's talk from yesterday, which impresses him.
        ~ impressedMeng = true
    }
    
  -> 1Session
* [I'm going to drink some coffee]
  You drink some coffee, energising you.
  ~ energy = energy + 1
  -> 1Session
* {friendsMade > friendsNeeded} [I see some friends I already made. I'll chat to them]
  You chat to your friends.
  ~ energy = energy - 1
  -> 1Session
* {socialBattery > 0} [I'm going to network]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You use the break to network. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> 1Session
* {socialBattery > 0} [I'm going to try and make some friends]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You go introduce yourself to some fellow students. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   // sam excursion
   { samExcursion:
    -> 1Session
    }
   { samExcursion == false:
   ~ samExcursion = true
    One of the friends you made, Sam, mentions that she isn't particularly interested in the next session and that her and few friends were going to walk to see the nearby Arco della Pace (a cool arch ~30min walk away). Do you want to go with her?
        
    * * [Yes] You go with Sam and her friends. It's a lot of fun!
           ~ funMemoriesMade = funMemoriesMade + 1
           ~ energy = energy - 1
           As you walk and chat to Sam, you learn that her supervisor is Professor W and that she's happy to make sure you make a good impression on him.
        
           She does just that when you return. Nice job! :D
          ~ metMeng = true
          ~ connectedWithMeng = true
          ~ impressedMeng = true
          -> lunch
    * * [No] You politely decline.
          -> 1Session
       }
* [I'm going to get some fresh air outside and stretch my legs]
  You have a lovely break outside in sunny Milan.
  -> 1Session
* {friendsMade > 3} [I'm going to ask my friend that I see over there to introduce me to the people they are chatting to] Your friend introduces you to the people they are with.
   ~ energy = energy - 1
   ~ temp acss = RANDOM(0,2)
   ~ temp friendss = RANDOM(1,2)
   {acss == 1: You meet 1 academic.}
   {acss > 1: You meet {acss} academics.}
   {friendss == 1: You make 1 new friend.}
   {friendss > 1: You make {friendss} new friends.}
   ~ friendsMade = friendsMade + friendss
   ~ academicsMet = academicsMet + acss
   -> 1Session
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> 1Session

= 1Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Refinement Types, Type Inference
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> lunch
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> lunch
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> lunch
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> lunch

= lunch
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
It's lunch!

This lunch, there's a DEI (Diversity, Equity, and Inclusion) lunch for your underrepresented group! Do you want to go?
* [Yes] You go to the DEI lunch. It is very rewarding and inspiring to meet all the lovely people there, and it helps you feel at home in this community.
    ~ energy = energy - 1
    ~ funMemoriesMade = funMemoriesMade + 1
    ~ temp acss = RANDOM(3,5)
    ~ temp friends = RANDOM(3,5)
   You meet {acss} academics and make {friends} friends!
   ~ friendsMade = friendsMade + friends
   ~ academicsMet = academicsMet + acss
  -> 2Session
* [No] You decide not to go.
  How do you spend your lunch instead?

    // random change at networking with Professor W
    ~ temp meng = RANDOM(1,5)
    * * {meng == 5 && metMeng == false && socialBattery > 1} [You see Professor W! You bite the bullet to go and introduce yourself]
       ~ socialBattery = socialBattery - 2
       ~ energy = energy - 1
       ~ metMeng = true
       You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
       { bxSessionAttended:
            You also mention that you attended his student's talk the day before, which impresses him.
            ~ impressedMeng = true
        }
       -> 2Session
    * * {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [You see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
       ~ socialBattery = socialBattery - 1
       ~ energy = energy - 1
       ~ connectedWithMeng = true
       { bxSessionAttended:
            You also discuss this student's talk from yesterday, which impresses him.
            ~ impressedMeng = true
        }
      -> 2Session
    * * {socialBattery > 0} [Making friends with your peers]
       ~ socialBattery = socialBattery - 1
       ~ energy = energy - 1
       ~ temp befriended = RANDOM(2,3)
       You spend lunch making friends with you peers. You make {befriended} friends!
       ~ friendsMade = friendsMade + befriended
       // sam excursion
       { samExcursion: -> 2Session}
       { samExcursion == false:
       ~ samExcursion = true
        One of the friends you made, Sam, mentions that she isn't particularly interested in the next session and that her and few friends were going to walk to see the nearby Arco della Pace (a cool arch ~30min walk away). Do you want to go with her?
           * * * [Yes] You go with Sam and her friends. It's a lot of fun!
               ~ funMemoriesMade = funMemoriesMade + 1
               ~ energy = energy - 1
            As you walk and chat to Sam, you learn that her supervisor is Professor W and that she's happy to make sure you make a good impression on him.
            
            She does just that when you return. Nice job! :D
              ~ metMeng = true
              ~ connectedWithMeng = true
              ~ impressedMeng = true
              -> afternoonBreak
           * * * [No] You politely decline.
              -> 2Session
           }
    
    * * {socialBattery > 0} [Networking with academics]
       ~ socialBattery = socialBattery - 1
       ~ energy = energy - 1
       ~ temp acs = RANDOM(2,3)
       You spend lunch networking. You meet {acs} academics!
       ~ academicsMet = academicsMet + acs
       -> 2Session
    * * [Trying all the desserts]
       You try all the desserts at lunch. The cannoli is simply divine.
       ~ funMemoriesMade = funMemoriesMade + 1
       ~ dessertsTried = dessertsTried + 1
       -> 2Session
    * * [I'm going to check in with how I'm feeling]
      You check in with yourself and your energy levels.
      Your energy is at {energy} / 9.
      Your social battery is at {socialBattery} / 3.
      -> 2Session
= 2Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Memory Models / Memory Management / Low-Level Languages
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> afternoonBreak
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> afternoonBreak
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> afternoonBreak
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> afternoonBreak

= afternoonBreak
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Afternoon Break
~ temp friendsNeeded = RANDOM(2,10)
// random change at networking with Professor W
~ temp meng = RANDOM(1,5)
* {meng == 5 && metMeng == false && socialBattery > 1} [I see Professor W! I'm going to bite the bullet to go and introduce myself]
   ~ socialBattery = socialBattery - 2
   ~ energy = energy - 1
   ~ metMeng = true
   You introduce yourself to Professor W. He's super lovely and gives you some advice about applying to Bristol. Well done! You achieved one of your conference goals :D
   { bxSessionAttended:
        You also mention that you attended his student's talk the day before, which impresses him.
        ~ impressedMeng = true
    }
   -> 3Session
* {meng == 5 && metMeng && connectedWithMeng == false && socialBattery > 0} [I see Professor W again, and decide to solidify him as a contact] You exchange emails with Professor W. Nice work! Hopefully, this will help you get your studentship.
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ connectedWithMeng = true
   { bxSessionAttended:
        You also discuss his student's talk from yesterday, which impresses him.
        ~ impressedMeng = true
    }
  -> 3Session
* [I'm going to drink some coffee]
  You drink some coffee, energising you.
  ~ energy = energy + 1
  -> 3Session
* {friendsMade > friendsNeeded} [I see some friends I made earlier. I'll chat to them]
  You chat to your friends.
  ~ energy = energy - 1
  -> 3Session
* {socialBattery > 0} [I'm going to network]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You use the break to network. You meet {acs} academics!
   ~ academicsMet = academicsMet + acs
   -> 3Session
* {socialBattery > 0} [I'm going to try and make some friends]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp befriended = RANDOM(2,3)
   You go introduce yourself to some fellow students. You make {befriended} friends!
   ~ friendsMade = friendsMade + befriended
   // sam excursion
   { samExcursion: -> 3Session}
   { samExcursion == false:
   ~ samExcursion = true
    One of the friends you made, Sam, mentions that she isn't particularly interested in the next session and that her and few friends were going to walk to see the nearby Arco della Pace (a cool arch ~30min walk away). Do you want to go with her?
       * * [Yes] You go with Sam and her friends. It's a lot of fun!
           ~ funMemoriesMade = funMemoriesMade + 1
           ~ energy = energy - 1
        As you walk and chat to Sam, you learn that her supervisor is Professor W and that she's happy to make sure you make a good impression on him.
        
        She does just that when you return. Nice job! :D
          ~ metMeng = true
          ~ connectedWithMeng = true
          ~ impressedMeng = true
          -> dinner
       * * [No] You politely decline.
          -> 3Session
       }
* [I'm going to get some fresh air outside and stretch my legs]
  You have a lovely break outside in sunny Milan.
  -> 3Session
* {friendsMade > 3} [I'm going to ask my friend that I see over there to introduce me to the people they are chatting to] Your friend introduces you to the people they are with.
   ~ energy = energy - 1
   ~ temp acss = RANDOM(0,2)
   ~ temp friendss = RANDOM(1,2)
   {acss == 1: You meet 1 academic.}
   {acss > 1: You meet {acss} academics.}
   {friendss == 1: You make 1 new friend.}
   {friendss > 1: You make {friendss} new friends.}
   ~ friendsMade = friendsMade + friendss
   ~ academicsMet = academicsMet + acss
   -> 3Session
* [I'm going to check in with how I'm feeling]
  You check in with yourself and your energy levels.
  Your energy is at {energy} / 9.
  Your social battery is at {socialBattery} / 3.
  -> 3Session

= 3Session
{energy < 1: You run out of energy. -> exhaustion}
{energy == 2: You notice that you're starting to get really tired.}
Session on Distributed Systems, Concurrency
* [I'm going to go and pay attention]
  You attend the session.
  ~ energy = energy - 1
  ~ sessionsAttended = sessionsAttended + 1
  ~ temp inspo = RANDOM(1,5)
  {inspo == 5:
    The session features a really interesting talk, which gives you a research idea!
    ~ ideasInspired = ideasInspired + 1
    }
  -> dinner
* {socialBattery > 0} [I'm not interested in this session, I'm going to network in the hallway]
   ~ socialBattery = socialBattery - 1
   ~ energy = energy - 1
   ~ temp acs = RANDOM(2,3)
   You take the "Hallway Track". You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> dinner
* [I'm going to sit quietly somewhere]
  You have a nice little break.
  -> dinner
* [I'm going to nap in my room]
  You have a refreshing nap.
  ~ energy = energy + 1
  -> dinner

= dinner
Dinner time!
* {energy < 1} You are exhausted from the day, so just grab some food and head to bed.
   -> endOfDay
* {energy > 0 && socialBattery > 0} [Socialisation in the evening is the best way to make lasting connections. I'm going to network!] You spend the evening networking over dinner.
    ~ temp acs = RANDOM(3,4)
   You meet {acs} academics!
   ~ academicsMet = academicsMet + acs 
   -> endOfDay
* {energy > 0 && socialBattery > 0 && friendsMade > 0} [I want so have some fun this evening! I grab some of my friends and we have dinner] You have a lovely evening with your friends.
    ~ funMemoriesMade = funMemoriesMade + 1
   -> endOfDay
* {energy > 0} [I'm going to have a chill evening with a take out and some netflix so that I can wind down from the big day and get a good sleep ready for tomorrow.] You have a nice chill evening.
    -> endOfDay

= endOfDay
Day Three complete!
Conference Complete!
  * [How did I do?] -> ending
 
// Endings:
// ---------------------------------------------------

=== exhaustion ===
Ultimately, you collapse from exhaustion. Try again. -> END

=== ending
Congrats for making it to the end of ICFP24!
Here's how you did in terms of your goals:

{(metMeng && bxSessionAttended): You achieved all your goals!}

{metMeng: 1. You met Professor W! | 1. Sadly, you did not achieve your first goal of making a connection with Professor W.}
{connectedWithMeng: Moreover, you ensured that you exchanged contact details. This will be very helpful for your aspirations of becoming a PhD student at the University of Bristol!}
{impressedMeng: In fact, you managed to impress Professor W, that University of Bristol studentship is surely in the bag!}

{bxSessionAttended: 2. You {not metMeng:did, however, catch | attended} the session with two BX talks! You now are more sure than ever that BX is what you want to do and have lots of ideas for your PhD project! | Sadly, you missed the main session of interest to you, which contained two BX talks.}

{not metMeng and not bxSessionAttended: You achieved neither of your goals. Try again. -> END}
     
Other things you achieved:

You attended {sessionsAttended} sessions!
You met {academicsMet} academics!
You made {friendsMade} friends!
You made {funMemoriesMade} fun memories!
The conference inspired {ideasInspired} ideas!
You tried {dessertsTried * 3} desserts!

// calculate how they did:

VAR overallScore = 0
// points for goals
{metMeng:
  ~ overallScore = overallScore + 5
  }
{connectedWithMeng:
  ~ overallScore = overallScore + 5
  }
{impressedMeng:
  ~ overallScore = overallScore + 10
  }
{bxSessionAttended:
  ~ overallScore = overallScore + 10
  }
// points things earned
~ overallScore = overallScore + sessionsAttended
~ overallScore = overallScore + academicsMet
~ overallScore = overallScore + friendsMade
~ overallScore = overallScore + funMemoriesMade
~ overallScore = overallScore + ideasInspired
// lose points for any zeros
{sessionsAttended == 0:
  ~ overallScore = overallScore -5
  Maybe you should attend more sessions next time.
  }
{academicsMet == 0:
  ~ overallScore = overallScore -5
  Maybe you should network more next time.
  }
{friendsMade == 0:
  ~ overallScore = overallScore -5
  Maybe you should make more friends next time.
  }
{funMemoriesMade == 0:
  ~ overallScore = overallScore -5
  You made no fun memories this conference. Next time remember to have fun!
  }
Overall Score: {overallScore}

Share how you did on Twitter with the hashtags ICFP24 and ConferenceCadet!

-> END