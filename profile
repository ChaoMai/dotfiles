# ld_library_path
# export LD_LIBRARY_PATH=~/Programs/local/lib/:${LD_LIBRARY_PATH}

# my bin
export PATH=~/.bin:$PATH

# TERM
export TERM=xterm-256color

# JAVA_HOME
JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export JAVA_HOME
export PATH=$JAVA_HOME:$PATH
JAVA_OPTS="-Dhttp.proxyHost=192.168.31.1 -Dhttp.proxyPort=1080 -Dhttps.proxyHost=192.168.31.1 -Dhttps.proxyPort=1080"
export JAVA_OPTS

# hadoop
HADOOP_HOME=/opt/hadoop-2.7.2
export HADOOP_HOME
export PATH=$HADOOP_HOME:$PATH

# hive
HIVE_HOME=/opt/apache-hive-2.0.0-bin
export HIVE_HOME
export PATH=$HIVE_HOME:$PATH

# gradle
GRADLE_HOME=/usr/share/java/gradle
export GRADLE_HOME

# ZooKeeper
ZOOKEEPER=/opt/zookeeper-3.4.9
export ZOOKEEPER

# gem
export PATH=~/.gem/ruby/2.3.0/bin:$PATH

# clang and clang++
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# vim
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim

# zsh
export SHELL=/usr/bin/zsh

# go workspace
export GOPATH=~/Documents/Codes/Current/GitHub/go_workspace
export PATH=$GOPATH/bin:$PATH

# gperftools lib
GPERFTOOLS_LIB=/usr/lib
export GPERFTOOLS_LIB

# maven
export MAVEN_OPTS="-Xms1024m -Xmx2048m"
