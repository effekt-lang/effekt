var $heap$data = {};
var $heap$pointer = 0;

function fresh$impl(init) {
    const ref = $heap$pointer++;
    $heap$data[ref] = init;
    return $effekt.pure(ref);
}

function put$impl(ref, value) {
    $heap$data[ref] = value;
    return $effekt.pure(null);
}

function get$impl(ref) {
    return $effekt.pure($heap$data[ref]);
}