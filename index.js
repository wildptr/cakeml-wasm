import fs from 'fs';

/*
function add_carry(a,b,c)
{
    a = BigInt.asUintN(64,a);
    b = BigInt.asUintN(64,b);
    //c = (c === 0n ? 0n : 1n);
    const sum = a+b+c;
    //console.log(`add_carry: ${a} + ${b} + ${c} = ${sum}`);
    const sum_w = BigInt.asUintN(64, sum);
    return [sum_w, (sum_w === sum ? 0n : 1n)]
}

function add_overflow(a,b)
{
    const sum = a+b;
    //console.log(`add_overflow sum: ${sum}`);
    const sum_w = BigInt.asIntN(64, sum);
    return [sum_w, (sum_w === sum ? 0n : 1n)]
}

function sub_overflow(a,b)
{
    const diff = a-b;
    //console.log(`sub_overflow diff: ${diff}`);
    const diff_w = BigInt.asIntN(64, diff);
    return [diff_w, (diff_w === diff ? 0n : 1n)]
}

function long_mul(a,b)
{
    a = BigInt.asUintN(64,a);
    b = BigInt.asUintN(64,b);
    const prod = a*b;
    //console.log(`long_mul: ${a} * ${b} = ${prod}`);
    const result = [prod>>64n, prod&0xffff_ffff_ffff_ffffn];
    //console.log(`=> ${result}`);
    return result;
}
*/

function long_div(a_hi,a_lo,b)
{
    const a = BigInt.asUintN(64,a_hi)<<64n | BigInt.asUintN(64,a_lo);
    //console.log(`long_div ${a}/${b}`);
    return [a/b, a%b]
}

var memory;
var dataview; // of memory
var argv;

function ffi_write(c, clen, a, alen)
{
    a = Number(a);
    const fd = Number(dataview.getBigInt64(Number(c), false/*big-endian*/));
    const n = memory[a]<<8|memory[a+1];
    const off = memory[a+2]<<8|memory[a+3];
    const nw = fs.writeSync(fd, memory, a+4+off, n);
    if (nw<0) {
        memory[a] = 1;
    }
    else {
        memory[a] = 0;
        memory[a+1] = nw>>8;
        memory[a+2] = nw&255;
    }
}

function ffi_read(c, clen, a, alen)
{
    a = Number(a);
    const fd = Number(dataview.getBigInt64(Number(c), false/*big-endian*/));
    const n = memory[a]<<8|memory[a+1];
    const nr = fs.readSync(fd, memory, a+4, n);
    if (nr<0) {
        memory[a] = 1;
    }
    else {
        memory[a] = 0;
        memory[a+1] = nr>>8;
        memory[a+2] = nr&255;
    }
}

function ffi_get_arg_count(c, clen, a, alen)
{
    a = Number(a);
    dataview.setInt16(a, argv.length, a, true/*little-endian*/);
}

function ffi_get_arg_length(c, clen, a, alen)
{
    a = Number(a);
    const i = dataview.getInt16(a, true/*little-endian*/);
    dataview.setInt16(a, argv[i].length, true/*little-endian*/);
}

function ffi_get_arg(c, clen, a, alen)
{
    a = Number(a);
    const i = dataview.getInt16(a, true/*little-endian*/);
    memory.set(new TextEncoder().encode(argv[i]), a);
}

function strlen(s)
{
    var i=0;
    while (memory[s+i] !== 0) i++;
    return i;
}

function ffi_open_in(c, clen, a, alen)
{
    c = Number(c);
    a = Number(a);
    const fd = fs.openSync(memory.slice(c, c+strlen(c)));
    if (fd >= 0) {
        memory[a] = 0;
        dataview.setBigInt64(a+1, BigInt(fd), false/*big-endian*/);
    }
    else {
        memory[a] = 1;
    }
}

function ffi_close(c, clen, a, alen)
{
    a = Number(a);
    const fd = Number(dataview.getBigInt64(Number(c), false/*big-endian*/));
    fs.closeSync(fd); // returns undefined
    memory[a] = 0;
}

function rt_exit(code)
{
    process.exit(Number(code));
}

// For debugging
function rt_tee(n)
{
    console.log(`# ${n}`);
    return n;
}

const imports = {
    host: {
        /*add_carry,add_overflow,sub_overflow,long_mul,*/
        long_div,
        ffi_write, ffi_read, ffi_open_in, ffi_close,
        ffi_get_arg_count, ffi_get_arg_length, ffi_get_arg,
        rt_exit, rt_tee
    }
}

/******************************************************************************/

async function load_wasm(filepath)
{
    const wasmBuffer = fs.readFileSync(filepath);
    const module = await WebAssembly.instantiate(wasmBuffer, imports);
    return module.instance;
}

argv = process.argv.slice(2);
load_wasm(process.argv[2]).then(instance => {
    //console.log("WASM module instantiated");
    dataview = new DataView(instance.exports.memory.buffer);
    memory = new Uint8Array(instance.exports.memory.buffer);
    //console.log(memory);
    instance.exports.wa_start();
}).catch(err => {
    console.error(err);
})
