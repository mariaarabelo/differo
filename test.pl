% gcd(X, 0, X).
% gcd(X, Y, G):- Y > 0,
%     XMY is X mod Y,
%     gcd(Y, XMY, G).

start:-
    write('Hello World!').
