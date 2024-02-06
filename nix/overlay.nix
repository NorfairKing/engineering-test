final: prev:
with final.lib;
with final.haskell.lib;
{
  scheduler = justStaticExecutables final.haskellPackages.scheduler;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        {
          "scheduler" = self.generateOptparseApplicativeCompletions [ "scheduler" ] (buildStrictly (self.callPackage ../scheduler { }));
        }
    );
  }
  );
}
