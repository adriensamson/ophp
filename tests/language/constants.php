<?php

namespace A\B;

echo __NAMESPACE__ . "\n";
echo __FILE__ . "\n";
echo __DIR__ . "\n";

function truc() {
    echo __FUNCTION__ . "\n";
}

truc();

class test {
    public static function test() {
        echo __FUNCTION__ . "\n";
        echo __CLASS__ . "\n";
        echo __METHOD__ . "\n";
    }
}

test::test();

