#!/usr/bin/perl
if ($#ARGV+1 == 0) {
  print "Usage: perl chopchop.pl arch_origen arch_destino\n(:\n";
} else {
  $file = $ARGV[0];
  $dest = $ARGV[1];
  open(IN, $file);
  open(OUT, ">$dest");
  while(<IN>) {
    chomp;
    s/[_\-;:.1234567890,$()"]//g;
    s/\n/ /g;
    s/  / /g;
    s/  / /g;
    s/  / /g;
    s/  / /g;
    s/  / /g;
    s/ /\n/g;
    print OUT "$_"; print OUT "\n";
  }
  close(IN);
  close(OUT);
}
