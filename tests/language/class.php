<?php

class Test {
    public $test = 'test';
    public static $hello = 'hello';
    
    public function hello($word) {
        echo "hello ".$word."\n";
    }
    
    public static function sayHello() {
        echo Test::$hello . "\n";
    }
    
    public static function lateHello() {
        echo static::$hello . "\n";
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

class Test2 extends Test {
    public static $hello = 'HELLO';
    
    public function hello($word) {
        echo "-----\n";
        parent::hello($word);
        echo "-----\n";
    }
}
echo "Test2\n";
$obj2 = new Test2();
$obj2->hello('world');
Test2::sayHello();
Test2::lateHello();

