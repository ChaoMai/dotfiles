# my bin
export PATH=/home/chaomai/.bin:$PATH

# JAVA_HOME
JAVA_HOME=/usr/lib/jvm/java-8-openjdk
export JAVA_HOME
export PATH=$JAVA_HOME:$PATH

# GRADLE_HOME
GRADLE_HOME=/usr/share/java/gradle
export GRADLE_HOME

# gem
export PATH=/home/chaomai/.gem/ruby/2.2.0/bin:$PATH

# clang and clang++
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

# vim
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim

# zsh
export SHELL=/usr/bin/zsh

# go workspace
export GOPATH=/home/chaomai/Documents/Codes/Current/GitHub/go_workspace
export PATH=$GOPATH/bin:$PATH

# gperftools lib
GPERFTOOLS_LIB=/usr/lib
export GPERFTOOLS_LIB

# spark
export PATH=/home/chaomai/.bin/spark-1.5.2-bin-hadoop2.6/bin:$PATH

# maven
export MAVEN_OPTS="-Xms1024m -Xmx2048m"
