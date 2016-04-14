# my bin
export PATH=/home/chaomai/.bin:$PATH

# TERM
export TERM=xterm-256color

# JAVA_HOME
JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export JAVA_HOME
export PATH=$JAVA_HOME:$PATH
JAVA_OPTS="-Dhttp.proxyHost=192.168.31.1 -Dhttp.proxyPort=1080 -Dhttps.proxyHost=192.168.31.1 -Dhttps.proxyPort=1080"
export JAVA_OPTS

# GRADLE_HOME
GRADLE_HOME=/usr/share/java/gradle
export GRADLE_HOME

# gem
export PATH=/home/chaomai/.gem/ruby/2.3.0/bin:$PATH

# clang and clang++
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# vim
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim

# zsh
export SHELL=/usr/bin/zsh

# go workspace
export GOPATH=/home/chaomai/Documents/Codes/Current/GitHub/go_workspace
export PATH=$GOPATH/bin:$PATH

# gperftools lib
GPERFTOOLS_LIB=/usr/lib
export GPERFTOOLS_LIB

# spark
SPARK_HOME=/home/chaomai/.bin/spark-1.6.0/bin
export PATH=$SPARK_HOME:$PATH

# maven
export MAVEN_OPTS="-Xms1024m -Xmx2048m"
