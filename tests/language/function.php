<?php

$a = 12;

function test($b) {
    global $a;
    return $a + $b;
}

echo test(3) . "\n";


function first(array $a) {
    return $a[0];
}

echo first(array(1)) ."\n";

function hello($hello = 'Hello') {
    echo $hello ."\n";
}

hello('Coucou');
hello();

