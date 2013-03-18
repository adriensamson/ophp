<?php

$add = function ($a, $b) { return $a + $b; };

echo $add(1, 1);
echo "\n";

$double = function ($a) use ($add) { return $add($a, $a); };

echo $double(1);
echo "\n";

