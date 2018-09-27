#Script to get the vHPC counts per layer from the output log file.
#
#Requires the starting and ending line numbers which may be obtained via
# grep commands. Example line numbers are given below, but must be changed
# for a particular experiment. 
#
#Ryan Kennedy 21 August 2018
#

#!/usr/bin/perl
#use warnings;
use strict;
use Tie::File;
 
my $file = $ARGV[0] or die "Need to get CSV file on the command line\n";
 
#line starts and ends
my @l0starts = ("510","2681942","5187040","7692489","10198074","12703523","15208704","17713427","20218695","22723706","25228101","27733006");
my @l0ends = ("554","2681986","5187084","7692533","10198118","12703567","15208748","17713471","20218739","22723750","25228145","27733050");
my @l1starts = ("556","2681988","5187086","7692535","10198120","12703569","15208750","17713473","20218741","22723752","25228147","27733052");
my @l1ends = ("575","2682007","5187105","7692554","10198139","12703588","15208769","17713492","20218760","22723771","25228166","27733071");
my @l2starts = ("577","2682009","5187107","7692556","10198141","12703590","15208771","17713494","20218762","22723773","25228168","27733073");
my @l2ends = ("579","2682011","5187109","7692558","10198143","12703592","15208773","17713496","20218764","22723775","25228170","27733075");

my @array;

tie @array, 'Tie::File', $file;

my $totalL0vHPCs = 0;
my $totalL1vHPCs = 0;
my $totalL2vHPCs = 0;
my @words;

for(my $j=0; $j<12; $j++) { #for each trial (12 here)
for(my $i=$l0starts[$j]; $i<=$l0ends[$j]; $i++) {
  @words = split /[=,]/, $array[$i];
  $totalL0vHPCs += ($words[3] * $words[5]);
}
print "Layer 0 Total $j: $totalL0vHPCs\n";
$totalL0vHPCs = 0;

}

for(my $j=0; $j<12; $j++) {
for(my $i=$l1starts[$j]; $i<=$l1ends[$j]; $i++) {
  @words = split /[=,]/, $array[$i];
  $totalL1vHPCs += ($words[3] * $words[5]);
}
print "Layer 1 Total $j: $totalL1vHPCs\n";
$totalL1vHPCs = 0;

}

for(my $j=0; $j<12; $j++) {
for(my $i=$l2starts[$j]; $i<=$l2ends[$j]; $i++) {
  @words = split /[=,]/, $array[$i];
  $totalL2vHPCs += ($words[3] * $words[5]);
}
print "Layer 2 Total $j: $totalL2vHPCs\n";
$totalL2vHPCs = 0;

}

