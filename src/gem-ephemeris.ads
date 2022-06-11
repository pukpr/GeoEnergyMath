package Gem.Ephemeris is

   procedure MOONPOS(TJD,GST : in Long_Float;
                     R,alpha,delt : out Long_Float);

   procedure JULDAT(Y,M,D : in Integer;
                    H : in Long_Float;
                    TJD : out Long_Float);

      -- calculates Julian date for the given calendar date and time.
      -- input calendar date : GREGORIAN
      -- output : Julian date
      -- (after Fliegel & Van Flandern)

end Gem.Ephemeris;
