#!/usr/bin/perl

## todo:
## specify what dirs are needed, where they're expected, etc.
## customize to take an input parameter "-input ref" or "-input test"
## adapt to run more than one benchmark per invocation?

##
## setup and run benchmark
##
## (c) 1997  Dirk Grunwald <grunwald@cs.colorado.edu>, University of Colorado
##      and  Artur Klauser <klauser@cs.colorado.edu>, Intel
##

##
## Bring variables into PERL from environment.
##
##  RUN       - value is "INST" or "PRE" or "RUN" or "POST".
##                INST: install binary 
##                PRE:  do prerun setup (e.g. copy input files)
##                RUN:  run actual simulation
##                POST: do cleanup (e.g. remove input/output files)
##
##  BENCHMARK - which binary to run (see -binaries)
##  BENCHDB   - benchmark database
##  DIRECTORY - directory name
##  SIMULATOR - simulator executable to use (absolute path)
##  CAPTURE   - capture stderr of program run in Out/$BENCHMARK.$suffix
##  ARGUMENTS - options to SIMULATOR (switches must be enclosed in quotes)
##

###########################################################################
## hpux/linux/NT specific settings go up here
$exp_dir = "./";
$bench_dir = "$exp_dir/bench";
$input_dir = "$exp_dir/input/ref";
$output_dir = "$exp_dir/output/ref";
$ext = "ss"  if (! defined $ext);
$OS = "hpux";
#$OS = "Windows_NT";
$cp = "cp";				# unix default
$rm = "rm -f -r";
$link = "ln -s";
$and = ";";
$endian = "big";

$verbose = 1;

local(@benchmarks);

## user-specific settings (if any) go here

###########################################################################

$usage_message =<<EOF;

Run sets up and runs a benchmark.

Options can either be passed as environment variables (long form) 
or on the command line (long or short form).
E.g., the following commands are equivalent:

$ setenv BENCHDB foo
$ Run

and:

$ Run -benchdb foo

Options:

Long        Short    Description
------------------------------------------------------------------------
binaries    bin      print a list of all known benchmarks
run                  run phases to execute; any combination of
                       INST  install binary 
                       PRE   do prerun setup
                       RUN   run actual simulation
                       POST  do cleanup
                       ALL   all of above (default)
benchdb     db       benchmark database file (describes benchmarks)
benchmark   bench    name of benchmark to run (from binaries list)
directory   dir      base directory to run benchmarks in
simulator   sim      simulator executable; none means run natively
arguments   args     options for simulator executable
capture     cap      capture stdout and stderr in file
preserve    pres     copy benchmark output files to Out directory
gzip        gz       on-the-fly gzip of captured output
verbose     v        rattle along as the script executes
compare     comp     compare files to expected outputs
help        h        this help message

Required options:
  benchdb, benchmark, directory

Operation:
  Run generates Out and Ran directories in the base run directory and
  optionally also creates the last level of the base run directory if it
  is not already there. Binaries and input files are temporarily copied
  into the base run directory during the run, which avoids interferences
  between multiple running instances. Captured output is generated in the
  Out subdirectory.  Successfull completion of the run is recorded in the
  Ran subdirectory and benchmarks will not be rerun if their Ran entry
  already exists.

EOF


&main();


sub main {
	&init();			# reads environment variables, inits tables
	&parse_args();
	&read_db();
	&list_benches()
		if (defined $print_binaries && $print_binaries);
	&check_vars();		# checks setting of $DIRECTORY, etc.
	chdir "$DIRECTORY";
	$RUN = " \U$RUN\E ";	# uppercase
	&setup_dirs();
	if ($RUN =~ / ALL /) {
		$RUN = " INST PRE RUN POST ";
	}

        foreach $benchmark (@benchmarks) {
            if (!$BINARIES{$benchmark}) {
                &mydie("Don't know how to install $benchmark\n");
            }
        }
        &vprint("Doing @benchmarks\n");
        &install_and_run(@benchmarks);

}

############################################################################

##
## Install and run a job
##

sub install_and_run
{
    local(@benchmarks) = @_;
    local(@binary,@runargs,@prerun,@postrun,$outfile);
    local(@loc, $cmd);
    local($benchmark);
    local($stdin_file) = "";

    foreach $i (0..$#benchmarks) {
        $benchmark = $benchmarks[$i];
	$binary[$i]	= $BINARIES{$benchmark};
	$runargs[$i]	= $RUN_ARGS{$benchmark};
        $runargs[$i] =~ s/<.*$//g;
	$outfile	= $OUT_FILE{$benchmark};
	$prerun[$i]	= $PRE_RUN{$benchmark};
	$postrun[$i]	= $POST_RUN{$benchmark};
	$loc[$i]	= "run.$benchmark";
        if ($STDIN_FILE{$benchmark}) {
            if ($stdin_file) {
                &mydie("STDIN is already used for $stdin_file, but $benchmark requires STDIN for $STDIN_FILE{$benchmark}\n");
            } else {
                $stdin_file = $STDIN_FILE{$benchmark};
            }
        }

	if ($RUN =~ / INST /) {
		&vprint("Installing $benchmark\n");
		unlink $loc[$i];
		$cmd = "$link $binary[$i] $loc[$i]\n";
		&vprint($cmd);
		system $cmd;
		if ($? != 0) {
			&mydie("Installation failed!\n");
		}
	}

	if ($RUN =~ / PRE /) {
		&vprint("Pre-run $benchmark\n");
		if (defined $prerun[$i]) {
			&vprint("$prerun[$i]\n");
			system $prerun[$i];
		}
	}
    } # end foreach

    if ($RUN =~ / RUN /) {
        # SAM -- added $ARGUMENTS and $suffix below
        # can't redir stdout and stderr
        # stdout already goes to file for each benchmark (see $BENCHDB)
        $cmd = "$SIMULATOR $ARGUMENTS";
        foreach $i (0..$#benchmarks) {
            $cmd .= " $loc[$i] $runargs[$i]";
        }
        if ($stdin_file) {
            $cmd .= " < $stdin_file";
        }
        if (defined $CAPTURE && $CAPTURE) {
            $cmd .= " > $outfile 2> Out/$benchmark.$suffix";
        } else {
            $cmd .= " > $outfile";
        }
        &vprint("Run @benchmarks\n");
        &vprint("$cmd\n");
        $start_time = time;
        system("$cmd");
        &elapsed_time($start_time);
        if ($compare) {
            system("perl compare.pl $outputdir/$outfile $outfile >>Ran/$benchmark");
        }
        if (defined $GZIP && $GZIP) {
            system("gzip Out/$benchmark.suffix");
        }
        ##
        ## ran ok?
        ##
        if ($? == 0) {
            # SAM -- want to distinguish between different
            # configs, so make the "been there, done that"
            # token for each benchmark unique per configuration
            &Touch("Ran/$benchmark.$suffix");
        } else {
            &vprint("****** program returns ERROR $?\n");
        }
    }

    if ($RUN =~ / POST /) {
        foreach $i (0..$#benchmarks) {
            $benchmark = $benchmarks[$i];
            &vprint("Post-run $benchmark\n");
            if (defined $PRESERVE) {
                &vprint("preserving output files\n");
                system("$cp $outfile Out/$outfile.$suffix");
            }
            if (defined $postrun[$i]) {
                &vprint("$postrun[$i]\n");
                system $postrun[$i];
            }
            unlink "$loc";
	} # end foreach
    } 
}

sub InstallAndRun
{
	local($benchmark) = @_;
	local($binary,$runargs,$prerun,$postrun);
	local($loc, $cmd);

	$binary		= $BINARIES{$benchmark};
	$runargs	= $RUN_ARGS{$benchmark};
	$outfile	= $OUT_FILE{$benchmark};
	$prerun		= $PRE_RUN{$benchmark};
	$postrun	= $POST_RUN{$benchmark};
	$loc		= "run.$benchmark";

	if ($RUN =~ / INST /) {
		&vprint("Installing $benchmark\n");
		unlink $loc;
		$cmd = "$link $binary $loc\n";
		&vprint($cmd);
		system $cmd;
		if ($? != 0) {
			&mydie("Installation failed!\n");
		}
	}

	if ($RUN =~ / PRE /) {
		&vprint("Pre-run $benchmark\n");
		if (defined $prerun) {
			&vprint("$prerun\n");
			system $prerun;
		}
	}

	if ($RUN =~ / RUN /) {
		# SAM -- added $ARGUMENTS and $suffix below
		# can't redir stdout and stderr
		# stdout already goes to file for each benchmark (see $BENCHDB)
		if (defined $CAPTURE && $CAPTURE) {
			$cmd = "$SIMULATOR $ARGUMENTS $loc $runargs > $outfile 2> Out/$benchmark.$suffix";
		} else {
			$cmd = "$SIMULATOR $ARGUMENTS $loc $runargs > $outfile";
		}
		&vprint("Run $benchmark\n");
		&vprint("$cmd\n");
		$start_time = time;
		system("$cmd");
		&elapsed_time($start_time);
		if ($compare) {
			system("perl compare.pl $outputdir/$outfile $outfile >>Ran/$benchmark");
		}
		if (defined $GZIP && $GZIP) {
			system("gzip Out/$benchmark.suffix");
		}
		##
		## ran ok?
		##
		if ($? == 0) {
			# SAM -- want to distinguish between different
			# configs, so make the "been there, done that"
			# token for each benchmark unique per configuration
			&Touch("Ran/$benchmark.$suffix");
		} else {
			&vprint("****** program returns ERROR $?\n");
		}
	}

	if ($RUN =~ / POST /) {
		&vprint("Post-run $benchmark\n");
		if (defined $PRESERVE) {
			&vprint("preserving output files\n");
			system("$cp $outfile Out/$outfile.$suffix");
		}
		if (defined $postrun) {
			&vprint("$postrun\n");
			system $postrun;
		}
		unlink "$loc";
	}
}

sub Touch {
	local($file) = @_;
	local($now) = time;
	if ( -f $file ) {
		utime $now, $now, $file;
	} else {
		open(FOO,">$file");
		close(FOO);
	}
}

sub PutEnv
{
	local($name, $value) = @_;
	$ENV{$name} = $value;
	&vprint( "$name = $ENV{$name} \n" );
}

sub mydie {
	local($string) = shift;
	print STDERR $string;
	exit(0);
}

sub vprint {
	print STDERR @_	if (defined $verbose && $verbose);
}

##
## need help?
##

sub usage {
	print STDERR $usage_msg;
	exit(0);
}

sub init {
	$RUN = "ALL";
	$RUN = $ENV{"RUN"}		if (defined $ENV{"RUN"});
	$BENCHMARK = $ENV{"BENCHMARK"}	if (defined $ENV{"BENCHMARK"});
	$BENCHDB = $ENV{"BENCHDB"}	if (defined $ENV{"BENCHDB"});
	$DIRECTORY = $ENV{"DIRECTORY"}  if (defined $ENV{"DIRECTORY"});
	$SIMULATOR = "";
	$SIMULATOR = $ENV{"SIMULATOR"}	if (defined $ENV{"SIMULATOR"});
	$ARGUMENTS = $ENV{"ARGUMENTS"}	if (defined $ENV{"ARGUMENTS"});
	$CAPTURE = $ENV{"CAPTURE"}	if (defined $ENV{"CAPTURE"});
	$GZIP = $ENV{"GZIP"}		if (defined $ENV{"GZIP"});
	%BINARIES = ();
	%RUN_ARGS = ();
	%PRE_RUN  = ();
	%POST_RUN = ();
	# CAUTION -- untested code ahead
	foreach $key (keys %ENV) {
		if ($key eq "OS") {
			$OS = $ENV{$key};
			if ($OS eq "Windows_NT") {
				$cp = "copy";
				$rm = "del";
				$link = $cp;
				$and = "&";
				$endian = "little";
				# add any other NT/x86-specific defs here
			}
			elsif ($OS eq "linux") {
				$endian = "little";
			}
		}
	}
}

sub parse_args {
	while ($#ARGV >= 0) {
		$arg = shift @ARGV;
		if ($arg eq "-binaries" || $arg eq "-bin" ) {
			$print_binaries = 1;
		}
		elsif ($arg eq "-run" ) {
			$RUN = shift @ARGV;
		}
		elsif ($arg eq "-benchmark" || $arg eq "-bench" ) {
                    $benchmarks[++$#benchmarks] = shift @ARGV;
                    # $BENCHMARK = shift @ARGV;
		}
		elsif ($arg eq "-benchdb" || $arg eq "-db") {
			$BENCHDB = shift @ARGV;
		}
		elsif ($arg eq "-directory" || $arg eq "-dir") {
			$DIRECTORY = shift @ARGV;
		}
		elsif ($arg eq "-simulator" || $arg eq "-sim") {
			$SIMULATOR = shift @ARGV;
		}
		elsif ($arg eq "-arguments" || $arg eq "-args") {
			$ARGUMENTS = shift @ARGV;
			$suffix = $ARGUMENTS;
			$suffix =~ s/\-//g;
			$suffix =~ s/\ //g;	# use this for output file
		}
		elsif ($arg eq "-capture" || $arg eq "-cap") {
			$CAPTURE = 1;
		}
		elsif ($arg eq "-preserve" || $arg eq "-PRES") {
			$PRESERVE = 1;
		}
		elsif ($arg eq "-gzip" || $arg eq "-gz") {
			$GZIP = 1;
		}
		elsif ($arg eq "-verbose" || $arg eq "-v") {
			$verbose = 1;
		}
		elsif ($arg eq "-compare" || $arg eq "-comp") {
			$compare = 1;
			print "WARNING: comparisons may be faulty\n"
		}
		elsif ($arg eq "-help" || $arg eq "-h") {
			&usage();
		}
		elsif ($arg =~ /^-.*/ ) {
			print STDERR "Don't understand the $arg option\n";
			&usage();
		}
		else {
			$, = " ";
			print STDERR "Don't understand command line args: $arg @ARGV\n";
			&usage();
;
		}
	}
}

##
## bring in the benchmark database
##

sub read_db {
	if (defined $BENCHDB) {
		if (-f $BENCHDB) {
			&vprint("reading benchmark database $BENCHDB\n");
			do "$BENCHDB"
				|| &mydie ("Error in benchmark database $BENCHDB\n$@");
		}
		else {
			&mydie("Can't find benchmark database $BENCHDB\n");
		}
	}
	else {
		&mydie("No benchmark database defined -- can't run!\n");
	}
}

sub list_benches {
	print "You can select these benchmarks:\n";
	foreach $bin (sort keys %BINARIES) {
		print "$bin\n";
	}
	exit(0);
}

sub check_vars {
	if (! defined $DIRECTORY) {
		&mydie("You must specify a directory\n");
	}
	# if (! defined $BENCHMARK) {
        if ($#benchmarks < 0) {
            &mydie("You must specify a benchmark\n");
	}
	if (! -f $SIMULATOR) {
		&mydie("You must specify a simulator\n");
	}
	
	if (! -d "$DIRECTORY") {
		mkdir("$DIRECTORY",0775) || 
			&mydie("Can not create $DIRECTORY subdirectory\n");
	}
}

##
## setup and check directory structure
##

sub setup_dirs {
	if (! -d "Ran") {
		mkdir("Ran",0775)
			|| &mydie("Can not create Ran subdirectory\n");
	}
	if (! -d "Out") {
		mkdir("Out",0775)
			|| &mydie("Can not create Out subdirectory\n");
	}
	if (! -d "Ref") {
		mkdir("Ref",0775)
			|| &mydie("Cannot create Ref subdirectory\n");
	}
	if ($compare && ! -d "Expected") {
		mkdir("Expected",0775)
			|| &mydie("Cannot create Expected subdirectory\n");
	}
}

sub elapsed_time {
	local($t1) = @_;
#	my(@lt) = localtime($t1);
#	my(@day) = ("Sun","Mon","Tue","Wed","Thu","Fri","Sat");
#	my(@month)=("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec");
	$t2 = time - $t1;
	$min = ($t2 / 60) % 60;
	$hour = $t2 / 3600 % 136556;
	$t_str = "$day[$lt[6]], $month[$lt[4]] $lt[3] at $lt[2]:$lt[1]:$lt[0]";
	open(BENCH, ">>Ran/$benchmark.$suffix");
	print BENCH "\nstart time:\t$t_str \nrun time:\t$t2 seconds ($hour hours, $min minutes)\n";
	print BENCH "program returned $?\n";
	close BENCH;
}



