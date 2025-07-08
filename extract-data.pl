#!/usr/bin/perl
use v5.038;
use File::Basename;

sub extract_data($fh)
{
    my $S = 0;
    my $data = '';
    while (<$fh>) {
        if ($S==0) {
            if (/^cake_bitmaps:$/) {
                $S=1;
            }
        }
        else {
            if (/^\s*\.quad (.*)/) {
                my @words = split(',',$1);
                foreach (@words) {
                    $data .= pack('Q', $_);
                }
            }
            else {
                return $data;
            }
        }
    }
}

my $wat = $ARGV[0];
my $S = $ARGV[1];
open (my $wat_fh, '<', $wat);
open (my $S_fh, '<', $S);

my $data = extract_data($S_fh);
my $data_size = length $data;

my $support = <<'.';
(func $split_hl (param i64) (result i64 i64)
  (return
    (i64.shr_u (local.get 0) (i64.const 32))
    (i64.and (local.get 0) (i64.const 0xffff_ffff))
  )
)

(func $split_lh (param i64) (result i64 i64)
  (return
    (i64.and (local.get 0) (i64.const 0xffff_ffff))
    (i64.shr_u (local.get 0) (i64.const 32))
  )
)

(func $long_mul (param $a i64) (param $b i64) (result i64 i64)
  (local $ah i64)
  (local $al i64)
  (local $bh i64)
  (local $bl i64)
  (local $ll_lo i64)
  (local $hl_hi i64)
  (local $t i64)
  (local $LO i64) ;; lower 64 bits of (a*b)

  (call $split_hl (local.get $a))
  (local.set $al)
  (local.set $ah)
  (call $split_hl (local.get $b))
  (local.set $bl)
  (local.set $bh)

  (i64.mul (local.get $al) (local.get $bl))
  (call $split_hl)
  (local.set $ll_lo)                        ;; ll_hi

  (i64.mul (local.get $al) (local.get $bh)) ;; ll_hi ; lh
  (i64.add)                                 ;; lh + ll_hi

  (i64.mul (local.get $bl) (local.get $ah)) ;; lh + ll_hi ; hl
  (call $split_lh)                          ;; lh + ll_hi ; hl_lo ; hl_hi
  (local.set $hl_hi)                        ;; lh + ll_hi ; hl_lo
  (i64.add)                                 ;; lh + ll_hi + hl_lo
  (local.tee $t)                            ;; ... = t

  (i64.const 32)
  (i64.shl)
  (i64.or (local.get $ll_lo))
  (local.set $LO)

  (local.get $t)
  (i64.const 32)
  (i64.shr_u)                               ;; t_hi
  (i64.add (local.get $hl_hi))              ;; t_hi + hl_hi
  (i64.mul (local.get $ah) (local.get $bh)) ;; t_hi + hl_hi ; hh
  (i64.add)                                 ;; t_hi + hl_hi + hh = HI

  (local.get $LO)                           ;; HI ; LO
)

(func $add_overflow (param $a i64) (param $b i64) (result i64 i64)
  (local $sum i64)
  (local.tee $sum (i64.add (local.get $a) (local.get $b)))
  (i64.and
    (i64.xor (i64.const -1)
      (i64.xor (local.get $a) (local.get $b))
    )
    (i64.xor (local.get $a) (local.get $sum))
  )
  (i64.const 63)
  (i64.shr_u)
)

(func $add_carry (param $a i64) (param $b i64) (param $carry i64) (result i64 i64)
  (local $sum i64)
  (select (i64.const 0) (i64.const 1) (i64.eqz (local.get $carry)))
  (i64.add (local.get $a) (local.get $b))
  (i64.add)
  (local.tee $sum)
  (i64.shr_u (i64.xor (local.get $a) (local.get $b)) (i64.const 63))
  (i32.wrap_i64)
  (if (result i64)
    (then
      (i64.shr_u (i64.xor (i64.xor (local.get $a) (local.get $b)) (local.get $sum)) (i64.const 63))
    )
    (else ;; sign(a) = sign(b)
      (i64.shr_u (local.get $a) (i64.const 63))
    )
  )
)

(func $sub_overflow (param $a i64) (param $b i64) (result i64 i64)
  (if (result i64 i64)
    (i64.eq (local.get $b) (i64.const 0x8000000000000000))
    (then
      (i64.sub (local.get $a) (local.get $b))
      (i64.xor (i64.const 1) (i64.shr_u (local.get $a) (i64.const 63)))
    )
    (else
      (call $add_overflow (local.get $a) (i64.sub (i64.const 0) (local.get $b)))
    )
  )
)
.

while (<$wat_fh>) {
    my @A = split /`([^`]*)`/;
    print($A[0]);
    for (my $i=1; $i<@A; $i+=2) {
        print(eval($A[$i]));
        print($A[$i+1]);
    }
    if (/\(memory/) {
        print '(data (i32.const 0) "';
        foreach (unpack('C*',$data)) {
            printf '\%02x', $_;
        }
        print "\")\n";
    }
}
