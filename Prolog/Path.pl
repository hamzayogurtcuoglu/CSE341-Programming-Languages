%part2 20.12.2018
%In the graph below you see the possible flights between some of the cities in Turkey.

% Hamza Yoğurtcuoğlu
% 171044086

%Begin All facts

flight(edirne,erzurum,5).
flight(erzurum,edirne,5).

flight(erzurum,antalya,2).
flight(antalya,erzurum,2).

flight(antalya,izmir,1).
flight(izmir,antalya,1).

flight(izmir,istanbul,3).
flight(istanbul,izmir,3).

flight(istanbul,ankara,2).
flight(ankara,istanbul,2).

flight(istanbul,trabzon,3).
flight(trabzon,istanbul,3).

flight(izmir,ankara,6).
flight(ankara,izmir,6).

flight(antalya,diyarbakir,5).
flight(diyarbakir,antalya,5).

flight(ankara,diyarbakir,8).
flight(diyarbakir,ankara,8).

flight(trabzon,ankara,6).
flight(ankara,trabzon,6).

flight(ankara,kars,3).
flight(kars,ankara,3).

flight(kars,gaziantep,3).
flight(gaziantep,kars,3).

%End All facts

%Begin Rules
route(X , Y , Z) :-  total_Cost(X , Y , Z , [])   ;   flight(X,Y,Z). %getting total cost of flight path
 
total_Cost(X , Y , Z , _) :-  flight(X , Y , Z).   %finding closest city end returning recursive

total_Cost(X , Y , K , M) :-  \+ member(X , M)  , flight(X , Z , F) , total_Cost(Z , Y , E , [X|M]) , X\=Y,  K is F + E.		
%summing up the total cost then  total cost finding recursively

%End Rules

