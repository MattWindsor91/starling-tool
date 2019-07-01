if [[ ! -e z3-z3-4.8.5/install/lib/libz3.so ]]; then
    wget https://github.com/Z3Prover/z3/archive/z3-4.8.5.zip
    unzip z3-4.8.5.zip
    cd z3-z3-4.8.5
    python scripts/mk_make.py  --prefix=`pwd`/install
    cd build
    make
    make install
fi

cd $TRAVIS_BUILD_DIR
