<?php

$a = 12;

function test($b) {
    global $a;
    return $a + $b;
}

echo test(3);

