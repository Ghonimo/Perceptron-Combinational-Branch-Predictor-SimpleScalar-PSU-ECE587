# Primes.pl	-- determines by brute force if the values fed to it are prime

LINE: while (<>) {
  chop;
  while ($_ =~ s/(.*) (.*)/$1$2/) {};
  while ($_ =~ s/(.*)x(.*)/$1$2/) {};
  $num = $_;
  for ($i = 2; $i < int($num / 2); $i++) {
    if ($num % $i == 0) {
      print "$num is not prime.\n";
      next LINE;
    }
  }
  print "$num is prime.\n";
}
