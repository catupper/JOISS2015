# 小町算の計算

入力：整数`a`と`i`を空白区切りで受け取る。
`a`は式の目標値を表す。

出力：1から`i`までの連続する自然数を、
隣同士をつなげて1つの整数にするか、
足し算、もしくは掛け算でつなげた式を出力する。
すべての式は、計算結果が`a`となる。

入力1:

    100 9

出力1:

    1*2*3 + 4 + 5 + 6 + 7 + 8*9
    1 + 2 + 3 + 4 + 5 + 6 + 7 + 8*9
    1*2*3*4 + 5 + 6 + 7*8 + 9
    12 + 3*4 + 5 + 6 + 7*8 + 9
    1 + 2*3 + 4 + 5 + 67 + 8 + 9
    1*2 + 34 + 5 + 6*7 + 8 + 9
    12 + 34 + 5*6 + 7 + 8 + 9
