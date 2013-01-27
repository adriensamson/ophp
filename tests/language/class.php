<?php

class Test {
    public $test = 'test';
    public static $hello = 'hello';
    
    public function hello($word) {
        echo "hello ".$word."\n";
    }
    
    public static function sayHello() {
        echo Test::$hello;
    }
}

$obj = new Test();

echo $obj->test;
echo "\n";

$obj->test = 'TEST';

echo $obj->test;
echo "\n";

$obj->hello('world');

Test::sayHello();

