all:
	piki -m ../piki/index.piki > tmp.md
	sed -e 's/html/md/' tmp.md > index.md
	rm -f tmp.md
	piki -m ../piki/beg.piki > beg.md
	piki -m ../piki/list.piki > list.md
