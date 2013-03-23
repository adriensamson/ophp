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

