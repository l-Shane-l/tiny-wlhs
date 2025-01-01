# Format all Haskell files
git ls-files '*.hs' '*.lhs' | while IFS= read -r file; do
	fourmolu -i "$file"
done

# Commit just the formatting changes
git add .
git commit -m "style: apply fourmolu formatting"

# Get the commit hash and add it to .git-blame-ignore-revs
git rev-parse HEAD >>.git-blame-ignore-revs
