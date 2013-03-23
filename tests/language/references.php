<?php

$a = 1;
$b = & $a;
$b = 2;
echo $a ."\n";

$array = array(12);
$array[1] = & $a;
$a = 3;
echo $array[1] . "\n";

function incr(&$a) {
    $a++;
}

$c = 3;
incr($c);
echo $c . "\n";

function &getRow(&$a, $k) {
    return $a[$k];
}

$d = array(1, 2);
$e =& getRow($d, 0);
$e = 2;

echo $d[0] . "\n";

