IRB.conf[:PROMPT][:GEM] = 
  { PROMPT_I: "◇: ",
    PROMPT_N: "◇: ",
    PROMPT_S: nil,
    PROMPT_C: "◆: ",
    RETURN:   "⇒ %s\n"
  };

IRB.conf[:PROMPT_MODE] = :GEM

require 'pp'
