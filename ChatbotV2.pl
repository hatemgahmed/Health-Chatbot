:- [food].
:- [read_sentence].


totalCal(1800).

readInputTillQuit:-
writeln('> Welcome to your personal assistant'),
write('> '),
readInputTillQuitHelp([],[]),!.

readInputTillQuitHelp(PQ,PR):-
res(X),
(last(X,?) -> select(?,X,Xnew), !; (last(X,.) -> select(.,X,Xnew))),
(Xnew = [quit] -> getbreakfast(PQ,PR,B),getlunch(PQ,PR,L),getdinner(PQ,PR,D),printReport(B,L,D),!;
      (\+isValid(Xnew) -> (writeln('> I can not understand you'),write('> '),readInputTillQuitHelp(PQ,PR)), !;
            ( isValid(Xnew) -> response(Xnew,PQ,PR,R),append(PQ,[Xnew],PQnew),append(PR,[R],PRnew),
                  write('> '),
                  ws(R),
                  writeln(""),
                  write('> '),
                  readInputTillQuitHelp(PQnew,PRnew)
                  ))).
wh([H]):-
write(H).
wh([H|T]):-
T\= [],write(H),write(,),
wh(T).

printReport(A,B,C):-

printReportHelp(A,Anew),
printReportHelp(B,Bnew),
printReportHelp(C,Cnew),

write("You had "), wh(Anew) , writeln(" for breakfast"),
write("You had "), wh(Bnew) , writeln(" for lunch"),
write("You had "), wh(Cnew) , writeln(" for dinner"),
write("Bye").

printReportHelp([],[-]).
printReportHelp(X,X):-
X\=[].

getbreakfast([],[],[]).
getbreakfast([H|T],[_|T1],B):-
H\=[i,ate,_,for,breakfast],
H\=[can, i, have, _ , for, breakfast],
getbreakfast(T,T1,B).

getbreakfast([[i,ate,D,for,breakfast]|T],[["Ok"]|T1],B):-
getbreakfast(T,T1,B1),
B=[D|B1].

getbreakfast([[can, i, have, D , for, breakfast]|T],[[you,can,have,D,for,breakfast]|T1],B):-
getbreakfast(T,T1,B1),
B=[D|B1].


getlunch([],[],[]).
getlunch([H|T],[_|T1],B):-
H\=[i,ate,_,for,lunch],
H\= [can, i, have, _ , for, lunch],
getlunch(T,T1,B).
getlunch([[i,ate,D,for,lunch]|T],[["Ok"]|T1],B):-
getlunch(T,T1,B1),
B=[D|B1].
getlunch([[can, i, have, D , for, lunch]|T],[[you,can,have,D,for,lunch]|T1],B):-
getlunch(T,T1,B1),
B=[D|B1].

getdinner([],[],[]).
getdinner([H|T],[_|T1],B):-
H\=[i,ate,_,for,dinner],
H\=[can, i, have, _ , for, dinner],
getdinner(T,T1,B).
getdinner([[i,ate,D,for,dinner]|T],[["Ok"]|T1],B):-
getdinner(T,T1,B1),
B=[D|B1].
getdinner([[can, i, have, D , for, dinner]|T],[[you,can,have,D,for,dinner]|T1],B):-
getdinner(T,T1,B1),
B=[D|B1].



%%%% ISVALID/1 %%%%


isValid([how, many, calories, does, _ , contain]).
isValid([what, does, _ , contain]).
isValid([can, i, have, _ , for, _]).
isValid([what, is, _ ]).
isValid([how, many, calories, do, i, have, left]).
isValid([what, kind, of, _, does, _, contain]).
isValid([is, _, a, _, in, _]).
isValid([what, can, i, have, for, _, that, contains, _]).
isValid([i , ate , _ , for , _ ]).
isValid([i, do , not , eat , _ ]).


%%%% FILTERPROP/2 %%%%

filterProp(Relation,Result):-
      setof((X,Y),prop(X,Relation,Y),Result).


%%%% MATCHFIRST/3 %%%%

matchFirst(_ , [] ,[]).

matchFirst(X,[(X,Y)|T],[Y-1|T2]):-
      matchFirst(X , T , T2).

matchFirst(X ,[(Z,Y)|T] ,[Y-0|T2]):-
      X \= Z ,
      matchFirst(X,T,T2).


%%%% MATCHSECOND/3 %%%%

matchSecond(_ , [] ,[]).

matchSecond(X,[(Y,X)|T],[Y-1|T2]):-
      matchSecond(X , T , T2).

matchSecond(X ,[(Y,Z)|T] ,[Y-0|T2]):-
      X \= Z ,
      matchSecond(X,T,T2).


%%%% MERGEMATCHLIST/3 %%%%

mergeMatchLists([],[],R,R).

mergeMatchLists([],L,Acc,R):-
      append(Acc,L,R).

mergeMatchLists(L,[],Acc,R):-
      append(Acc,L,R).

mergeMatchLists([X-N1|T1],[X-N2|T2],Acc,R):-
      append(A,[(X-N3)|T3],Acc),
      N is N1+N2+N3,
      append(A,[(X-N)|T3],AccNew),
      mergeMatchLists(T1,T2,AccNew,R).

mergeMatchLists([X-N1|T1],[X-N2|T2],Acc,R):-
      \+ append(_,[(X-_)|_],Acc),
      N is N1+N2,
      append(Acc,[X-N],AccNew),
      mergeMatchLists(T1,T2,AccNew,R).

mergeMatchLists(L1,L2,R):-
      mergeMatchLists(L1,L2,[],R).


%%%% BESTMATCHES/2 %%%%

bestMatches([],_,R,R).

bestMatches([X-N|T] , Max , Acc , R):-
      N = Max,
      append(Acc,[X],AccNew),
      bestMatches(T , Max , AccNew ,R).

bestMatches([_-N|T] , Max , Acc ,R):-
      N < Max,
      bestMatches(T , Max , Acc , R).

bestMatches([X-N|T] , Max , _ , R):-
      N > Max,
      AccNew = [X],
      bestMatches(T , N ,AccNew,R).

bestMatches(ML,BL):-
      bestMatches(ML,0,[],BL).


%%%% BESTMATCHESMIN/3 %%%%

bestMatchesMin([],_,R,R).

bestMatchesMin([X-N|T],Min,Acc,R):-
      N = Min,
      append(Acc,[X],AccNew),
      bestMatchesMin(T,Min,AccNew,R).

bestMatchesMin([_-N|T],Min,Acc,R):-
      N \= Min,
      bestMatchesMin(T,Min,Acc,R).

bestMatches(ML,Min,BL):-
      bestMatchesMin(ML,Min,[],BL).


%%%% FOODCAL/2 %%%%

foodCal(F,C):-
      prop(F,contain,C,cal).

foodCal(F,C):-
      setof(X,prop(F,contain,X),R),
      foodCalHelp(R,0,C).

foodCalHelp([],R,R).

foodCalHelp([H|T],Acc,C):-
      prop(H,contain,X,cal),
      AccNew is Acc + X,
      foodCalHelp(T,AccNew,C).


%%%% FOODCALLIST/2 %%%%

foodCalList(FL,C):-
      foodCalListHelp(FL,0,C).

foodCalListHelp([],R,R).

foodCalListHelp([H|T],Acc,C):-
      foodCal(H,X),
      AccNew is Acc + X,
      foodCalListHelp(T,AccNew,C).


%%%% CALCCALORIES/4 %%%%

calcCalories(F,PQ,PR,C):-
      calcCaloriesList(PQ,PR,0,R1),
      foodCal(F,R2),
      totalCal(X),
      C is X-(R1+R2).

calcCaloriesList([],[],R,R).

calcCaloriesList([[i,ate,X,for,_]|T1] , [["Ok"]|T2] , Acc , R):-
      foodCal(X,C),
      AccNew is Acc + C,
      calcCaloriesList(T1,T2,AccNew,R).

calcCaloriesList([[can, i, have, X , for, _]|T1] , [[you,can,have,X,for,_]|T2] , Acc , R):-
      foodCal(X,C),
      AccNew is Acc + C,
      calcCaloriesList(T1,T2,AccNew,R).

calcCaloriesList([H|T1],[_|T2],Acc,R):-
      H \= [i,ate,_,for,_],
      H \= [can, i, have, _ , for, _],
      calcCaloriesList(T1,T2,Acc,R).

calcCaloriesList([[i,ate,_,for,_]|T1],[H2|T2],Acc,R):-
      H2 \= ["Ok"],
      calcCaloriesList(T1,T2,Acc,R).

calcCaloriesList([[can, i, have, _ , for, _]|T1],[H2|T2],Acc,R):-
      H2 \= [you,can,have,_,for,_],
      calcCaloriesList(T1,T2,Acc,R).


%%%% GETDIFFANSWER/5 %%%%

getDiffAnswer(_,[],[],[R|_],R).

getDiffAnswer(_,_,_,[],["I",told,you,that,before]).

getDiffAnswer(Q,[Q|T1],[QR|T2],CR,R):-
      Q \= [how,many,calories,do,i,have,left],
      select(QR,CR,CRnew),
      getDiffAnswer(Q,T1,T2,CRnew,R).

getDiffAnswer(Q,[H1|T1],[_|T2],CR,R):-
      Q \= H1,
      getDiffAnswer(Q,T1,T2,CR,R).



%%%% RESPONSEO/4 %%%%

responseO(Q,PQ,PR,LR):-
candidate(Q,PQ,PR,CR),
countLooper(Q,PQ,PR,CR,LR).


count(_,_,[],[],0).

count(Q,X,[Q|T1],[H2|T2],N):-
count(Q,X,T1,T2,N1),
member(X,H2),
N is N1+1.

count(Q,X,[Q|T1],[H2|T2],N):-
count(Q,X,T1,T2,N),
\+member(X,H2).

count(Q,X,[H1|T1],[_|T2],N):-
Q \= H1,
count(Q,X,T1,T2,N).


countLooper(_,_,_,[],[]).

countLooper(Q,PQ,PR,[H|T],R):-
countLooper(Q,PQ,PR,T,R1),
count(Q,H,PQ,PR,N),
R=[H-N|R1].


%%%% RESPONSE/4 %%%%

response(Q,PQ,PR,R):-
candidate(Q,PQ,PR,CR),
\+length(CR,0),
getDiffAnswer(Q,PQ,PR,CR,R).


response([how,many,calories,do,i,have,left],PQ,PR,[C,"Calories"]):-
calcCaloriesList(PQ,PR,0,R1),
totalCal(X),
C is X-R1.

response(Q,_,_,["Ok"]):-
Q=[i,ate,_,for,_];
Q=[i,do,not,eat,_].

response(Q,PQ,PR,["Nothing",from,what,i,know]):-
candidate(Q,PQ,PR,CR),
length(CR,0).

response(Q,PQ,PR,["I",do,not,know]):-
\+candidate(Q,PQ,PR,_).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QUESTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


%%%% Q(a)

candidate(Q,_,_,[[R,"Calories"]]):-
        Q = [how, many, calories, does, F , contain],
        foodCal(F,R).

%%%% Q(b)

candidate(Q,_,_,CR):-
        Q = [what, does, F , contain],
        setof([X],prop(F,contain,X),CR).

%%%% Q(c)

candidate(Q,PQ,PR,[[you,can,have,F,for,FC]]):-
        Q = [can, i, have, F , for, FC],
        \+prop(F,not,FC),
        calcCalories(F,PQ,PR,C),
        C>=0.

candidate(Q,PQ,PR,[["No"]]):-
        Q = [can, i, have, F , for, FC],
        \+prop(F,not,FC),
        calcCalories(F,PQ,PR,C),
        C<0.

candidate(Q,_,_,[[F,is,not,suitable,for,FC]]):-
        Q = [can, i, have, F , for, FC],
        prop(F,not,FC).

%%%% Q(d)

candidate(Q,_,_,[[X]]):-
        Q = [what, is, F],
        prop(F,is,X).


%%%% Q(e)


        
%%%% Q(f)

candidate([what, kind, of, FC, does, F, contain],_,_,CR):-
        
        findall([A],(prop(F,contain,A),prop(A,is,FC)),CR).



%%%% Q(g)

candidate(Q,_,_,[["Yes"]]):-
        Q = [is, X, a, Y, in, Z],
        prop(Z,contain,X),
        prop(X,is,Y).

candidate(Q,_,_,[["No"]]):-
        Q = [is, X, a, Y, in, Z],
        (\+prop(Z,contain,X);
        \+prop(X,is,Y)).


%%%% Q(h)

candidate(Q,_,_,CR):-
      Q = [what, can, i, have, for, FC, that, contains, F],
      
      setof([A],(prop(A,contain,F),\+prop(A,not,FC)),CR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END QUESTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


%%%% LISTORDERDESC/2 %%%%

listOrderDesc(LP,OLP):-
        reverseIn(LP,LPnew),
        keysort(LPnew,X),
        reverse(X,Y),
        reverseIn(Y,OLP).

reverseIn([],[]).

reverseIn([X-Y|T],[Y-X|T1]):-
        reverseIn(T,T1).


%%%% FOODFROMHISTORY/2 %%%%

foodFromHistory([],[]).

foodFromHistory([H|T],L):-
        H \= [i,ate,_,for,_],
        foodFromHistory(T,L).

foodFromHistory([[i,ate,X,for,_]|T1],[X|T2]):-
        foodFromHistory(T1,T2).


%%%% GETUNLIKEDINGREDIENTS/2 %%%%

getUnlikedIngredients([],[]).

getUnlikedIngredients([H|T],FL):-
        H \= [i,do,not,eat,_],
        getUnlikedIngredients(T,FL).

getUnlikedIngredients([[i,do,not,eat,X]|T1],[X|T2]):-
        getUnlikedIngredients(T1,T2).