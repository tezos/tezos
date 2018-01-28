# Contributor: Natanael Copa <ncopa@alpinelinux.org>
# Maintainer:
pkgname=leveldb
pkgver=1.18
pkgrel=0
pkgdesc="A fast and lightweight key/value database library by Google"
url="https://github.com/google/leveldb"
arch="all"
license="BSD"
depends=""
depends_dev=""
makedepends="$depends_dev snappy-dev"
install=""
subpackages="$pkgname-dev $pkgname-doc"
source="$pkgname-$pkgver.tar.gz::https://github.com/google/$pkgname/archive/v$pkgver.tar.gz"

_builddir="$srcdir"/leveldb-$pkgver

build() {
	cd "$_builddir"
	make || return 1
}

package() {
	cd "$_builddir"
	mkdir -p "$pkgdir"/usr/lib "$pkgdir"/usr/share/doc
	cp -a lib*.so* "$pkgdir"/usr/lib || return 1
	cp -a include "$pkgdir"/usr/ || return 1
	cp -a doc "$pkgdir"/usr/share/doc/$pkgname || return 1

}

md5sums="73770de34a2a5ab34498d2e05b2b7fa0  leveldb-1.18.tar.gz"
sha256sums="4aa1a7479bc567b95a59ac6fb79eba49f61884d6fd400f20b7af147d54c5cee5  leveldb-1.18.tar.gz"
sha512sums="3d9c55a7bf8692914784ec33c273704ce9978496b071c7b661708f049d0d4ccd51a44441f50c3e536725caeb9896575192f52708a4bb1c0222cecdeec89919a3  leveldb-1.18.tar.gz"
