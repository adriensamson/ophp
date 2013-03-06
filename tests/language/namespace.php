<?php

namespace A\B;

class C {
    public function __construct() { echo "C\n"; }
}

$a = new C();

namespace A;

$b = new B\C();

$c = new \A\B\C();

