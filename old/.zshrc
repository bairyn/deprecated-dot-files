#!/usr/bin/env zsh

###############################################
# IMPORTANT
###############################################

# Techniques:
#
# 0: Alias for arbitrary processing of command line arguments!  Just use #2
# with a comment!  (fc -ln $[HISTCMD])
#
# 1: Commands in variables; portable (but unneeded string technique (c.f. #3) is not):
#        code=$(base64<<$'\n'
#          echo hello    'one two    three'
#        )
#        command="eval eval \"\$(base64 -d <<< $code)\""
#
#        $=command args
#
#        In bash, and zsh after `setopt no_sh_word_split` or `set -o no_sh_word_split`
#        $command args
#        Definitely prefer "$command"; much more portable.
#
# 2: zsh-only pseudo-quines:
#        # Can embed in single line to do something with the current command
#        # line.
#        echo "$(fc -ln $(( 1 + ${${=$(fc -lr)}[1]} )) )"                                                                                                               │
#
#        # ...TIL.  About HISTCMD and `$[]` = `$(())`.
#        # echo $(fc -ln $[HISTCMD])
#
# 3: `'`: `)`: zsh-only multiline string quoting with escapable parentheses, optional expansion (analogous to `'`):
#        # NO EXPANSION:
#
#        # Variable assignment.  Omit quotes to split the `()`-quoted text into
#        # words
#        foo="$(cat<<$'\n'
#          foo
#          $this_is_literal
#          bar
#        )"
#
#        # Command argument.
#        echo hello there "$(cat<<$'\n'
#          foo
#          bar
#        )" quux
#
#        # EXPANSION:
#
#        echo hello there "$((printf '%s' ${(e)==:-"${cat}"})<<cat<<$'\n'
#          foo
#          this is expanded: $variable
#          bar
#        )" quux
#
#        # By nesting this technique, support for a fixed maximum number of
#        # parentheses.  TODO: see if you can combine this technique with
#        # others too!  (TODO: expand on this paragraph.)
#
# 4: `"`: `} after an odd-numbered "`: zsh-only expanded string quoting ended by `}` not between an even number of unescaped double quotes, which are removed (analogous to `"`):
#        echo hello there "${==:-"
#          foo
#          bar
#        "} quux
#
# 5: `\\`: `}` zsh-only unquoted word joining string quoting ended by `}`, analogous to `\\`.
#        echo hello there ${==:-
#          foo
#          bar
#        } quux
#
#        echo hello there ${==:-hello there     again 'foo bar  quux'}
#
# 6: TODO: script for!! :D:
#   
#    # untested code golf
#    #env VISUAL=${==${==:-'sed -nri '${(j | )^argv}}:-"${VISUAL-"$EDITOR"}"} vidir
#    env VISUAL=${==${==:-'sed -nri '${(j | )^argv/(b#)(m#)(*)^s(.).*\1.*\1/$MATCH;p}}:-"${VISUAL-"$EDITOR"}"} vidir

# TODO: learn how completion works so that, on "lt *", you get a menu on first
# tab press, and then actually complete.







local source_filter='cat'
local source_filepath='/etc/zsh/zshrc.bev'

if [[ -e "${source_filepath}" ]]; then
  source =($=source_filter "$source_filepath")
else
  local valid="$(sed -nre's/[[:space:]]//g;p' << $'\n'
    9203fc08f9742ef2988953c010b4a3f77d09c9b867a1d9b494f603f18005b5c36a50787a620be007c501fb95e7aa22fdfd76b35b170a5d13ad9a064aec25d7c2
  )"
  local file_contents_src="$(cat << $'\0'
    aWYgKCggJHsrX0xPQURFRF9ldGNfenNocmNfYmV2fSApKTsgdGhlbgogIHJldHVybgpmaQoKc2V0
    IC1vIG5vX3Vuc2V0IC1vIG5vX2Nsb2JiZXIgLW8gZXJyX3JldHVybiAtbyBwaXBlX2ZhaWwKCmRl
    Y2xhcmUgLWdhciBfTE9BREVEX2V0Y196c2hyY19iZXYKCmZ1bmN0aW9uIGRwa2ctbGlzdCB7CiAg
    bG9jYWwgZ3JlcF9vcHRpb25zPSctRSAtLXRleHQgLS1jb2xvcj1hdXRvIC1pJwogIGdyZXBfb3B0
    aW9ucys9IiQoCiAgICByZWFkIC10MCAtZXJ1MCB8fCA6CiAgKSIKICBncmVwX29wdGlvbnM9IiQo
    CiAgICByZWFkIC10MCAtZXJ1MCB8fCBwcmludGYgJyVzJyAiJD09Z3JlcF9vcHRpb25zIgogICki
    CgogIGxvY2FsIGNtZAogIGNtZD0kez09Oi1kcGtnLXF1ZXJ5IC1XZiBcJ1wke1BhY2thZ2V9XFxu
    XCcke2FyZ3Y6KyIgfCBncmVwICQ9PWdyZXBfb3B0aW9ucyAiJHtePWFyZ3Z9fX0KCiAgbG9jYWwg
    cHJlY21kCiAgaWYgeyByZWFkIC10MCAtcnUwIHByZWNtZCA7fSBjbWQ9IiQ9PXByZWNtZCAkeyhx
    LSk9PWNtZH0iCiAgZXZhbCAiJD09Y21kIgp9CgpmdW5jdGlvbiBkcGtnLWxpc3RfaGVscCB7CiAg
    cHJpbnRmICclMiRzJTEkc1xuPT09PT09PT09XG4nICdPdmVydmlldy4nICcnCgogIHByaW50ZiAk
    JyAqICVzICVzXFxuJ1wKICAgICJTeW5vcHNpczogIiAiJHsocS0pJDB9IHNlYXJjaGVzIGluc3Rh
    bGxlZCBwYWNrYWdlcy4iCiAgICAiQWxpYXNlczogICIgIlRoaXMgc2NyaXB0IGRlZmluZXMgbm8g
    YWxpYXNlcyBmb3IgJHsocS0pJDB9LiIKCiAgcHJpbnRmICclMiRzJTEkc1xuPT09PT09PT09XG4n
    ICdFeGFtcGxlcy4nICQnXG5cbn5+flxuXG4nCgogIHByaW50ZiAkJyAqICVzIGRwa2ctcXVlcnkt
    bGlzdCVzXFxuJ1wKICAgICJMaXN0IGFsbDogICAgICAgICIgJCcnXAogICAgIkNvbnRhaW5zIGdo
    YzogICAgIiAkJyBnaGMnXAogICAgImFkZCBncmVwIGZsYWdzOiAgIiAkJyA8PDwgXCctLWNvbG9y
    PW5ldmVyJ1wnIGdoY1wKICAgICJzZXQgZ3JlcCBmbGFnczogICIgJCcgPDw8Ljw8PCBcJy1FIC0t
    b25seS1tYXRjaGluZyAtaVwnIGdoYydcCiAgICAiU2ltcGxlIG1hdGNoOiAgICAiICQnIF53ZXNu
    b3RoJCdcCiAgICAid2Vzbm90aCAmJiBkYXRhOiAiICQnIHdlc25vdGggZGF0YSdcCiAgICAiSGls
    aWdodCAnZGF0YSc6ICAiICQnIHdlc25vdGggXCcoZGF0YSk/XCcnXAogICAgIkhpbGlnaHQgJ2Rh
    dGEnOiAgIiAkJyB3ZXNub3RoIFwnKHxkYXRhKVwnJ1wKICAgICJQaXBpbmcgb3V0cHV0OiAgICIg
    JCcgXndlc25vdGgkIHwgdGVlJ1wKICAgICJubyBlc2NhcGUgY29kZXM6ICIgJCcgXndlc25vdGgk
    IHwgcmVtb3ZlLWVzY2FwZS1jb2RlcydcCiAgICAib3I6ICAgICAgICAgICAgICAiICQnIChnaGN8
    Y2FiYWwtaW5zdGFsbHx3ZXNub3RoKSdcCiAgICAid2hhdCB3b3VsZCBydW46ICAiICQnIDw8PCBc
    Jy0tdGhpcy1mbGFnXCcgPDw8IFwnLS1vci10aGF0XCcgPDw8IFwnZWNobyAtRSBnaGMgY2FiYWwt
    aW5zdGFsbFwnIHdlc25vdGgnXAogIDsKfQoKCmZ1bmN0aW9uIHJlbW92ZS1lc2NhcGUtY29kZXMg
    ewogICgoICQjIDw9IDAgKSkgfHwgeyBlY2hvICd0b28gbWFueSBhcmdzJzsgcmV0dXJuIDEgO30K
    CiAgIyBUaGFua3MgPGh0dHA6Ly9zZXJ2ZXJmYXVsdC5jb20vYS83MTI4OS8yNzgxMTQ+IQogIHNl
    ZCAtciAicy9ceDFCXFsoWzAtOV17MSwyfSg7WzAtOV17MSwyfSk/KT9bbXxLXS8vZyIKfQoKZnVu
    Y3Rpb24gewogIGxvY2FsIG9sZF9wcm9tcHQ9IiR7UFJPTVBULX0iCgogIGZ1bmN0aW9uIGJldi1w
    cm9tcHQtc2V0LXByZS11cGRhdGUgewogICAgbG9jYWwgcmVwbGFjZWRfcHJvbXB0PSIke1BST01Q
    VC19IgogICAgZXhwb3J0IFBST01QVD0iJHtvbGRfcHJvbXB0fSIKICAgIG9sZF9wcm9tcHQ9IiR7
    cmVwbGFjZWRfcHJvbXB0fSIKICB9Cn0KCiMjc291cmNlIC9ldGMvenNoL25ld3VzZXIuenNocmMu
    cmVjb21tZW5kZWQKIyNwcm9tcHQgcHdzCiMjIE9ubHkgbG9hZCBjb21wbGV0aW9uIHBhcnQgb2Yg
    dGhlIGZpbGUuCiNzb3VyY2UgPShzZWQgLW5yZSAnL15hdXRvbG9hZC4qY29tcGluaXQvLCRwJyAn
    L2V0Yy96c2gvbmV3dXNlci56c2hyYy5yZWNvbW1lbmRlZCcpCgoKIyBUT0RPOiB0cnVuY2F0aW9u
    IQpkZWNsYXJlIC1neCBOT19CRVZfUFJPTVBUPSIke05PX0JFVl9QUk9NUFQtIiQoKGZhbHNlKSki
    fSIKZnVuY3Rpb24gYmV2LXByb21wdCB7CiAgc2V0IC1vIHByb21wdF9zdWJzdAoKICAjIDwhLS0g
    fn5+fn5+fn4gIC0tPgogICMgQ29tbW9uIGNvbXBvbmVudHMuCiAgIyA9PT09PT09PT09PT09PT09
    PT0KICAjICogYCV74oCmJX1gOiByYXcgZXNjYXBlIGNvZGUuCiAgbG9jYWwgIiQoZWNobyByZXNl
    dF9lc2NhcGVfY29kZXMgICAgICAgKT0iJCcle1xlWzAwbSV9JwogIGxvY2FsICIkKGVjaG8gX2Zt
    dF9jb25kaXRpb25hbCAgICAgICAgICk9IiQnJSUoJXMuJXMuJXMpJwoKICBsb2NhbCAiJChlY2hv
    IF9oZWFydCAgICAgICAgICAgICAgICAgICApPSIkJ+KdpCcKICBsb2NhbCAiJChlY2hvIF9oZWFy
    dGwgICAgICAgICAgICAgICAgICApPSIkJyV7XGVbMzFtJX0nCiAgbG9jYWwgIiQoZWNobyBfaGVh
    cnRyICAgICAgICAgICAgICAgICAgKT0iJCcle1xlWzM5bSV9JwogIGxvY2FsICIkKGVjaG8gaGVh
    cnQgICAgICAgICAgICAgICAgICAgICk9JHtfaGVhcnRsfSR7X2hlYXJ0fSR7X2hlYXJ0cn0iCiAg
    bG9jYWwgIiQoZWNobyBfYmV2ZWxvcG1lbnRfYWRtaW5faGVhcnRsKT0iJCcle1xlWzQxbSV9Jwog
    IGxvY2FsICIkKGVjaG8gX2JldmVsb3BtZW50X2FkbWluX2hlYXJ0cik9IiQnJXtcZVs0OW0lfScK
    ICBsb2NhbCAiJChlY2hvIF9iZW5ldm9sZW50X2FkbWluX2hlYXJ0ICApPSIkJ/CfkpMnICAjIOKd
    piAjIOKdoyAjIPCfkpUgICMg8J+SkwogIGxvY2FsICIkKGVjaG8gYmVuZXZvbGVudF9hZG1pbl9o
    ZWFydCAgICk9JHtfYmV2ZWxvcG1lbnRfYWRtaW5faGVhcnRsfSVCJHtfYmVuZXZvbGVudF9hZG1p
    bl9oZWFydH0lYiR7X2JldmVsb3BtZW50X2FkbWluX2hlYXJ0cn0iCiAgbG9jYWwgIiQoZWNobyBf
    cHJlaGVhcnQgICAgICAgICAgICAgICAgKT0ke2hlYXJ0fSIKICBsb2NhbCAiJChlY2hvIF9wcmVo
    ZWFydGwgICAgICAgICAgICAgICApPSIkJyV7XGVbMzVtJX0nCiAgbG9jYWwgIiQoZWNobyBfcHJl
    aGVhcnRyICAgICAgICAgICAgICAgKT0iJCcle1xlWzM5bSV9JwogIGxvY2FsICIkKGVjaG8gcHJl
    aGVhcnQgICAgICAgICAgICAgICAgICk9JHtfcHJlaGVhcnRsfSR7X2hlYXJ0fSR7X3ByZWhlYXJ0
    cn0iCiAgbG9jYWwgIiQoZWNobyBwcmViZW5ldm9sZW50X2FkbWluX2hlYXJ0KT0ke19oZWFydH0i
    CgogIGxvY2FsICIkKGVjaG8gaXRhbGljc19iZWdpbiAgICAgICAgICAgICk9IiQnJXtcZVswM20l
    fScKICBsb2NhbCAiJChlY2hvIGl0YWxpY3NfZW5kICAgICAgICAgICAgICApPSIkJyV7XGVbMjNt
    JX0nCgogIGxvY2FsICIkKGVjaG8gY3Vyc29yX3NhdmUgICAgICAgICAgICAgICk9IiQnJXtcZVtz
    JX0nCiAgbG9jYWwgIiQoZWNobyBjdXJzb3JfcmVzdG9yZSAgICAgICAgICAgKT0iJCcle1xlW3Ul
    fScKCiAgIyMgLUJvbGQtIGlmIHJlZ3VsYXIgdXNlci4gIFJlZCBpZiByb290LgogICMjbG9jYWwg
    IiQoZWNobyBjbGlfbG9va3MpPSIkJyUoIS4le1xlWzMxOzA3OzA0bSV9LiV7XGVbMDFtJX0pJwog
    ICNsb2NhbCAiJChlY2hvIGNsaV9sb29rcyk9IiQnJSghLiV7XGVbMzE7MDc7MDRtJX0uJXtcZVsw
    MG0lfSknCiAgI2xvY2FsICIkKGVjaG8gY2xpX2xvb2tzKT0iJCclKCEuJXtcZVszMTswNzswNG0l
    fSAgICAgICAgICAgICAgICAgICAgICAgICV7XGVbMjREJX0uJXtcZVswMG0lfSknCiAgIyMgVW5k
    ZXJsaW5lIHRoZSBmaXJzdCBjaGFyYWN0ZXIuCiAgI2xvY2FsICIkKGVjaG8gY2xpICAgICAgKT0i
    ICMkJyV7XGVbc1xlWzA0bSV94oCmJXtcZVs2RFxlW3UlfScKCiAgIyB+fn5+fn5+fn5+fn4KICAj
    IExlZnQgcHJvbXB0LgogICMgPT09PT09PT09PT09CgogICNsb2NhbCAiJChlY2hvIGluX2NsaSAg
    ICk9IiQnJSghLiV7XGVbMzE7MDc7MDRtICAgICAgICAgICAgICAgICAgICAgICAgXGVbMjREJX0u
    JXtcZVswMG0lfSknCiAgbG9jYWwgIiQoZWNobyBpbl9jbGkgICApPSIgIyQnJSghLiV7XGVbMzE7
    MDc7MDRtICAgICAgICAgICAgICAgICAgICAgICAgXGVbMjREJX0uJXtcZVswMG0lfSknCgogICNs
    b2NhbCAiJChlY2hvIHByaXZpbGVnZV9pbmRpY2F0b3IpPSIkJyUoIS4jLiQocHJpbnRmIFxcXCcl
    LjBzXFxcXG5cXFwnICQoc2VxICciJHtTSExWTDotMH0iJCcpIHwgc2VkIC1yZSJcXCQhcy8uKlxc
    XFxuLyciJHtwcmVoZWFydH0iJy8iIC1lIlxcJHMvLipcXFxcbi8ke2hlYXJ0fS8iKSR7aGVhcnR9
    KScKICAjbG9jYWwgIiQoZWNobyBwcml2aWxlZ2VfaW5kaWNhdG9yKT0iJCclKCEuIy4nIiR7aGVh
    cnR9IicpJwogIGxvY2FsICIkKGVjaG8gcHJpdmlsZWdlX2luZGljYXRvcik9JChyZXBlYXQgJFtT
    SExWTC0xXSBwcmludGYgIiRfZm10X2NvbmRpdGlvbmFsIiAnIScgIiRwcmViZW5ldm9sZW50X2Fk
    bWluX2hlYXJ0IiAiJHByZWhlYXJ0IjsgcHJpbnRmICIkX2ZtdF9jb25kaXRpb25hbCIgJyEnICIk
    YmVuZXZvbGVudF9hZG1pbl9oZWFydCIgIiRoZWFydCIpIgogICMgKiBgJUJgOiBib2xkIG1vZGUu
    CiAgIyAqIGRhdGUtdGltZSBlc2NhcGVzOgogICMgICAqIGAlRGA6IHN0cmZ0aW1lKDMpIGZvcm1h
    dHRlZCBzdHJpbmc6CiAgIyAgICAgKiBgJUg6YCBob3VyICAgKDI0KQogICMgICAgICogYCVJOmAg
    aG91ciAgICgxMikKICAjICAgICAqIGAlUDpgIGFtL3BtCiAgIyAgICAgKiBgJU06YCBtaW51dGUK
    ICAjICAgICAqIGAlUzpgIHNlY29uZHMKCiAgbG9jYWwgIiQoZWNobyB0aW1lICAgICApPSIkJyVC
    JUR7JUk6JU0lUH0lYjolRHslU30gJwoKICBsb2NhbCAiJChlY2hvIHVzZXIgICAgICk9IiQnJW4n
    CgogICMgKiBgJSFgOiBjdXJyZW50IGhpc3RvcnkgbnVtYmVyLgogIGxvY2FsICIkKGVjaG8gaF9i
    cmllZiAgKT0iJCcle1xlWzJtXGVbM20lfSQodGFpbCAtYyA0IDw8PCAiJFtISVNUQ01EXSIgfCBz
    ZWQgLXIgXCdzLy4oLi4pL+KAplxcMS9cJykle1xlWzIzbVxlWzIybSV9ICcKCiAgIyAqIGAlVWA6
    IHVuZGVybGluZS4KICAjICogYCVNYDogaG9zdG5hbWUuCiAgI2xvY2FsICIkKGVjaG8gaG9zdCAg
    ICAgKT0iJCdAJVUle1xlWyciJChob3N0bmFtZWN0bCAtLXN0YXRpYyBzdGF0dXMgfCBzaGE1MTJz
    dW0gfCBvZCAtdiAtQSBuIC1sIHwgdHIgLWQgJCcgXHRcbicgfCBzZWQgLXIgInMvXFxcYC4qXCcv
    JiAlIDggKyAzMC87cSIgfCBiYykiJCdtJX0lTSV7XGVbMzltJX0ldScKICBsb2NhbCAiJChlY2hv
    IGhvc3QgICAgICk9IiQnQCVVJXtcZVsnIiQoaG9zdG5hbWVjdGwgLS1zdGF0aWMgc3RhdHVzIHwg
    b2QgLXYgLUEgbiAtbCB8IHRyIC1kICQnIFx0XG4nIHwgc2VkIC1yICJzL1xcXGAuKlwnLyYgJSA4
    ICsgMzAvO3EiIHwgYmMpIiQnbSV9JU0le1xlWzM5bSV9JXUnCgogIGxvY2FsICIkKGVjaG8gcHdk
    X2FsbCAgKT0iJCc6JX4nCiAgbG9jYWwgIiQoZWNobyBwd2QgICAgICApPSIkJyUoNn4uJS0yZC/i
    gKYvJTJkLiV+KScKCiAgIyAqIGAlamA6IG51bWJlciBvZiBqb2JzLgogIGxvY2FsICIkKGVjaG8g
    am9icyAgICAgKT0iJCclKDFqLiVCJXtcZVswM20lfSglaikle1xlWzIzbSV9JWIuKScKCiAgbG9j
    YWwgIiQoZWNobyBzcGFjaW5nICApPSIkJyAnCgogIGV4cG9ydCBMUFJPTVBUPSIke3Jlc2V0X2Vz
    Y2FwZV9jb2Rlc30ke3RpbWV9JHtoX2JyaWVmfSR7dXNlcn0ke2hvc3R9JHtwd2R9JHtqb2JzfSR7
    cHJpdmlsZWdlX2luZGljYXRvcn0ke3NwYWNpbmd9JHtyZXNldF9lc2NhcGVfY29kZXN9JHtpbl9j
    bGl9IgoKICAjIH5+fn5+fn5+fn5+fn4KICAjIFJpZ2h0IHByb21wdC4KICAjID09PT09PT09PT09
    PT0KCiAgbG9jYWwgIiQoZWNobyBoaXN0b3J5X251bWJlciAgICAgKT0iJCclIScKCiAgIyAqIGRh
    dGUtdGltZSBlc2NhcGVzOgogICMgICAqIGAlRGA6IHN0cmZ0aW1lKDMpIGZvcm1hdHRlZCBzdHJp
    bmc6CiAgIyAgICAgKiBgJUg6YCBob3VyICAgKDI0KQogICMgICAgICogYCVJOmAgaG91ciAgICgx
    MikKICAjICAgICAqIGAlUDpgIGFtL3BtCiAgIyAgICAgKiBgJVo6YCB0aW1lIHpvbmUgbmFtZSBv
    ciBhYmJyZXZpYXRpb24KICAjICAgICAqIGAlejpgIG51bWVyaWMgdGltZSB6b25lOiBbKy1daGht
    bQogIGxvY2FsICIkKGVjaG8gc2lkZV90aW1lKT0iJCclRHslSDolTX1AJVogKCV6KScKCiAgbG9j
    YWwgIiQoZWNobyBleGl0X2NvZGUpPSIkJyUoMD8uLiV7XGVbMDsxOzMxbSV9XFwkPzolPyV7XGVb
    MDswM20lfSApJwoKICAjICogYCVlYDogZXZhbHVhdGlvbiBkZXB0aC4KICBsb2NhbCAiJChlY2hv
    IGVsdmwxICAgICk9IiQnZXZhbC1sdmw6JWUgJwogIGxvY2FsICIkKGVjaG8gX2VsdmwxICAgKT0i
    JCdldmFsLWx2bDolZScKICBsb2NhbCAiJChlY2hvIGVsdmwyXzIgICk9IiQnJSgyZS4le1xlWzMy
    bSV9JyIkX2VsdmwxIiQnJXtcZVszOW0lfSAuKScKICBsb2NhbCAiJChlY2hvIGVsdmwyXzMgICk9
    IiQnJSgzZS4le1xlWzMzbSV9JyIkX2VsdmwxIiQnJXtcZVszOW0lfSAuJyIkZWx2bDJfMiIkJykn
    CiAgbG9jYWwgIiQoZWNobyBlbHZsMl80ICApPSIkJyUoNGUuJUIlVSV7XGVbMzNtJX0nIiRfZWx2
    bDEiJCcle1xlWzM5bSV9JXUlYiAuJyIkZWx2bDJfMyIkJyknCiAgbG9jYWwgIiQoZWNobyBlbHZs
    ICAgICApPSRlbHZsMl80IgoKICAjICogYCVMYDogc2hlbGwgbmVzdGluZyBsZXZlbC4KICBsb2Nh
    bCAiJChlY2hvIHNobHZsMSAgICk9IiQnbHZsOiVMICcKICBsb2NhbCAiJChlY2hvIF9zaGx2bDEg
    ICk9IiQnbHZsOiVMJwogIGxvY2FsICIkKGVjaG8gc2hsdmwyICAgKT0iJCclKDJMLiVCJXtcZVsz
    NG1cZVs5NW0lfSciJF9zaGx2bDEiJCcle1xlWzM5bSV9JWIgLiciJHNobHZsMSIkJyknCiAgbG9j
    YWwgIiQoZWNobyBzaGx2bDMgICApPSIkJyUoM0wuJUIle1xlWzM1bSV9JyIkX3NobHZsMSIkJyV7
    XGVbMzltJX0lYiAuJyIkc2hsdmwyIiQnKScKICBsb2NhbCAiJChlY2hvIHNobHZsNCAgICk9IiQn
    JSg0TC4lQiVVJXtcZVszNm0lfSciJF9zaGx2bDEiJCcle1xlWzM5bSV9JXUlYiAuJyIkc2hsdmwz
    IiQnKScKICBsb2NhbCAiJChlY2hvIHNobHZsICAgICk9JHNobHZsNCIKCiAgIyAqIGAlIWA6IGN1
    cnJlbnQgaGlzdG9yeSBudW1iZXIuCiAgIyAqIGAlaWA6IGN1cnJlbnQgbGluZSBudW1iZXIuCiAg
    I2xvY2FsICIkKGVjaG8gaGlzdF9pbmZvKT0iJCcoJWkpICUhJwogIGxvY2FsICIkKGVjaG8gaGlz
    dF9pbmZvKT0iJCcoJSEpICQocHJpbnRmIFwnJTNzXCcgJHs9PUxJTkVOTy0kXCcle1xlWzMxbSV9
    XFwkTElORU5PPyV7XGVbMzltJX1cJ30pJwoKICBleHBvcnQgUlBST01QVD0iJHtyZXNldF9lc2Nh
    cGVfY29kZXN9JHtpdGFsaWNzX2JlZ2lufSR7ZXhpdF9jb2RlfSR7ZWx2bH0ke3NobHZsfSU+4oCm
    PiR7aGlzdF9pbmZvfSU+PiR7aXRhbGljc19lbmR9JHtyZXNldF9lc2NhcGVfY29kZXN9IgoKICAj
    IH5+fn5+fn5+fn5+fn5+fn5+fn4KICAjIEZpbmlzaGluZyByb3V0aW5lcy4KICAjID09PT09PT09
    PT09PT09PT09PT0KCiAgZXhwb3J0IFBST01QVD0iJHtMUFJPTVBUfSIKfQoKZnVuY3Rpb24gewog
    IGxvY2FsIC1pIG5vX2Jldl9wcm9tcHQ9IiR7Tk9fQkVWX1BST01QVC0iJCgoZmFsc2UpKSJ9Igog
    IGlmICgoICEkbm9fYmV2X3Byb21wdCApKTsgdGhlbgogICAgYmV2LXByb21wdAogIGZpCn0KCmxv
    Y2FsIHNvdXJjZV9maWx0ZXI9J3NlZCAtbnJlIC9eYXV0b2xvYWQuKmNvbXBpbml0LywkcCcKbG9j
    YWwgc291cmNlX2ZpbGVwYXRoPScvZXRjL3pzaC9uZXd1c2VyLnpzaHJjLnJlY29tbWVuZGVkJwoK
    aWYgW1sgLWUgIiR7c291cmNlX2ZpbGVwYXRofSIgXV07IHRoZW4KICBzb3VyY2UgPSgkPXNvdXJj
    ZV9maWx0ZXIgIiRzb3VyY2VfZmlsZXBhdGgiKQplbHNlCiAgbG9jYWwgdmFsaWQ9IiQoc2VkIC1u
    cmUncy9bWzpzcGFjZTpdXS8vZztwJyA8PCAkJ1xuJwogICAgOTEzYWE2NTkwODU3MzMzMThjNjYx
    MzcwMWQ5NjQ3N2ZlZjg0MzgxMmI5NzU5Y2IyZGFhZTQwNDE3NzJkNzJmYjYxY2UyYzI5Mjg5YzQ2
    ZjA4NDJlY2VhNTk2NTUwYjYyNzRmNjQzNTFjMjY2ZWUzOTRhMTUxM2M4YTRjNjYxM2QKICApIgog
    IGxvY2FsIGZpbGVfY29udGVudHNfc3JjPSIkKGNhdCA8PCAkJ1wwJwogICAgSXlCVFpYUWdkWEFn
    ZEdobElIQnliMjF3ZEFvS1lYVjBiMnh2WVdRZ0xWVjZJSEJ5YjIxd2RHbHVhWFFLY0hKdmJYQjBh
    VzVwZEFwdwogICAgY205dGNIUWdZV1JoYlRFS0NuTmxkRzl3ZENCb2FYTjBhV2R1YjNKbFlXeHNa
    SFZ3Y3lCemFHRnlaV2hwYzNSdmNua0tDaU1nVlhObAogICAgSUdWdFlXTnpJR3RsZVdKcGJtUnBi
    bWR6SUdWMlpXNGdhV1lnYjNWeUlFVkVTVlJQVWlCcGN5QnpaWFFnZEc4Z2Rta0tZbWx1Wkd0bAog
    ICAgZVNBdFpRb0tJeUJMWldWd0lERXdNREFnYkdsdVpYTWdiMllnYUdsemRHOXllU0IzYVhSb2FX
    NGdkR2hsSUhOb1pXeHNJR0Z1WkNCegogICAgWVhabElHbDBJSFJ2SUg0dkxucHphRjlvYVhOMGIz
    SjVPZ3BJU1ZOVVUwbGFSVDB4TURBd0NsTkJWa1ZJU1ZOVVBURXdNREFLU0VsVAogICAgVkVaSlRF
    VTlmaTh1ZW5Ob1gyaHBjM1J2Y25rS0NpTWdWWE5sSUcxdlpHVnliaUJqYjIxd2JHVjBhVzl1SUhO
    NWMzUmxiUXBoZFhSdgogICAgYkc5aFpDQXRWWG9nWTI5dGNHbHVhWFFLWTI5dGNHbHVhWFFLQ25w
    emRIbHNaU0FuT21OdmJYQnNaWFJwYjI0NktpY2dZWFYwYnkxawogICAgWlhOamNtbHdkR2x2YmlB
    bmMzQmxZMmxtZVRvZ0pXUW5DbnB6ZEhsc1pTQW5PbU52YlhCc1pYUnBiMjQ2S2ljZ1kyOXRjR3hs
    ZEdWeQogICAgSUY5bGVIQmhibVFnWDJOdmJYQnNaWFJsSUY5amIzSnlaV04wSUY5aGNIQnliM2hw
    YldGMFpRcDZjM1I1YkdVZ0p6cGpiMjF3YkdWMAogICAgYVc5dU9pb25JR1p2Y20xaGRDQW5RMjl0
    Y0d4bGRHbHVaeUFsWkNjS2VuTjBlV3hsSUNjNlkyOXRjR3hsZEdsdmJqb3FKeUJuY205MQogICAg
    Y0MxdVlXMWxJQ2NuQ25wemRIbHNaU0FuT21OdmJYQnNaWFJwYjI0NktpY2diV1Z1ZFNCelpXeGxZ
    M1E5TWdwbGRtRnNJQ0lrS0dScAogICAgY21OdmJHOXljeUF0WWlraUNucHpkSGxzWlNBbk9tTnZi
    WEJzWlhScGIyNDZLanBrWldaaGRXeDBKeUJzYVhOMExXTnZiRzl5Y3lBawogICAgZXloekxqb3VL
    VXhUWDBOUFRFOVNVMzBLZW5OMGVXeGxJQ2M2WTI5dGNHeGxkR2x2YmpvcUp5QnNhWE4wTFdOdmJH
    OXljeUFuSndwNgogICAgYzNSNWJHVWdKenBqYjIxd2JHVjBhVzl1T2lvbklHeHBjM1F0Y0hKdmJY
    QjBJQ1ZUUVhRZ0pYQTZJRWhwZENCVVFVSWdabTl5SUcxdgogICAgY21Vc0lHOXlJSFJvWlNCamFH
    RnlZV04wWlhJZ2RHOGdhVzV6WlhKMEpYTUtlbk4wZVd4bElDYzZZMjl0Y0d4bGRHbHZiam9xSnlC
    dAogICAgWVhSamFHVnlMV3hwYzNRZ0p5Y2dKMjA2ZTJFdGVuMDllMEV0V24wbklDZHRPbnRoTFhw
    QkxWcDlQWHRCTFZwaExYcDlKeUFuY2pwOAogICAgV3k1ZkxWMDlLaUJ5T253OUtpQnNPbnc5S2lj
    S2VuTjBlV3hsSUNjNlkyOXRjR3hsZEdsdmJqb3FKeUJ0Wlc1MUlITmxiR1ZqZEQxcwogICAgYjI1
    bkNucHpkSGxzWlNBbk9tTnZiWEJzWlhScGIyNDZLaWNnYzJWc1pXTjBMWEJ5YjIxd2RDQWxVMU5q
    Y205c2JHbHVaeUJoWTNScAogICAgZG1VNklHTjFjbkpsYm5RZ2MyVnNaV04wYVc5dUlHRjBJQ1Z3
    SlhNS2VuTjBlV3hsSUNjNlkyOXRjR3hsZEdsdmJqb3FKeUIxYzJVdAogICAgWTI5dGNHTjBiQ0Jt
    WVd4elpRcDZjM1I1YkdVZ0p6cGpiMjF3YkdWMGFXOXVPaW9uSUhabGNtSnZjMlVnZEhKMVpRb0tl
    bk4wZVd4bAogICAgSUNjNlkyOXRjR3hsZEdsdmJqb3FPaW82YTJsc2JEb3FPbkJ5YjJObGMzTmxj
    eWNnYkdsemRDMWpiMnh2Y25NZ0p6MG9JMklwSUNNbwogICAgV3pBdE9WMGpLU285TUQwd01Uc3pN
    U2NLZW5OMGVXeGxJQ2M2WTI5dGNHeGxkR2x2YmpvcU9tdHBiR3c2S2ljZ1kyOXRiV0Z1WkNBbgog
    ICAgY0hNZ0xYVWdKRlZUUlZJZ0xXOGdjR2xrTENWamNIVXNkSFI1TEdOd2RYUnBiV1VzWTIxa0p3
    bz0KICApIgogIGxvY2FsIGZpbGVfY29udGVudHM9JycKICBsb2NhbCBsaW5lCiAgPDwoYmFzZTY0
    IC1kIC1pIDw8KHByaW50ZiAnJXMnICIkZmlsZV9jb250ZW50c19zcmMiKSkgXAogICAgd2hpbGUg
    cmVhZCAtcnUwIGxpbmU7IGRvCiAgICAgIGZpbGVfY29udGVudHMrPSIkez09bGluZX0iJCdcbicK
    ICAgIGRvbmUKCiAgZGVjbGFyZSAtZ2EgdmlydHVhbF9maWxlX25ld3VzZXJfenNocmNfcmVjb21t
    ZW5kZWQKICB2aXJ0dWFsX2ZpbGVfbmV3dXNlcl96c2hyY19yZWNvbW1lbmRlZCs9IiR7ZmlsZV9j
    b250ZW50c30iCgogIGxvY2FsIGNoZWNrc3VtPSIkKCAgPj4oc2VkIC1ucmUncy9eW1s6c3BhY2U6
    XV0qKFteWzpzcGFjZTpdXSspLiokL1wxL3AnKSBcCiAgICAgICAgICAgICAgICAgICAgc2hhNTEy
    c3VtIFwKICAgICAgICAgICAgICAgICAgICAgIDw8KHByaW50ZiAnJXMnICIkZmlsZV9jb250ZW50
    cyIpCiAgICAgICAgICAgICAgICAgICkiCgogIGlmIFtbICIke2NoZWNrc3VtfSIgIT0gIiR7dmFs
    aWR9IiBdXTsgdGhlbgogICAgMT4mMiB7CiAgICAgIHByaW50ZiAkJyVzOiAlcyBcJyVzXCc6ICVz
    XFxuJyBcCiAgICAgICAgIiQwIiBcCiAgICAgICAgIlNvcnJ5LCBub3Qgc291cmNpbmcgdmlydHVh
    bCBmaWxlIiBcCiAgICAgICAgIiRzb3VyY2VfZmlsZXBhdGgiIFwKICAgICAgICAidGhlIGNoZWNr
    c3VtIGRvZXNuJ3QgbWF0Y2ghIgogICAgICBwcmludGYgJCcgIENoZWNrc3VtOiAlc+KAplxuJyAi
    JHtjaGVja3N1bVsxLDMyXX0iCiAgICAgIHByaW50ZiAkJyAgRXhwZWN0ZWQ6ICVz4oCmXG4nICIk
    e3ZhbGlkWzEsMzJdfSIKICAgIH0KICAgIGZhbHNlCiAgZWxzZQogICAgcHJpbnRmICQnJXM6IGZp
    bGUgXCclc1wnIGRvZXNuXCd0IGV4aXN0LlxuVmlydHVhbGx5IHNvdXJjaW5nIGFmdGVyIGZpbHRl
    cmluZyBcJyVzXCcuXG4nIFwKICAgICAgIiQwIiBcCiAgICAgICIke3NvdXJjZV9maWxlcGF0aH0i
    IFwKICAgICAgJ3ZpcnR1YWxfZmlsZV9uZXd1c2VyX3pzaHJjX3JlY29tbWVuZGVkJwogICAgc291
    cmNlID0oJD1zb3VyY2VfZmlsdGVyIDw8KHByaW50ZiAnJXMnICIkZmlsZV9jb250ZW50cyIpKQog
    IGZpCmZpCgoKIyNzb3VyY2UgL2V0Yy96c2gvbmV3dXNlci56c2hyYy5yZWNvbW1lbmRlZAojI3By
    b21wdCBwd3MKIyMgT25seSBsb2FkIGNvbXBsZXRpb24gcGFydCBvZiB0aGUgZmlsZS4KI3NvdXJj
    ZSA9KHNlZCAtbnJlICcvXmF1dG9sb2FkLipjb21waW5pdC8sJHAnICcvZXRjL3pzaC9uZXd1c2Vy
    LnpzaHJjLnJlY29tbWVuZGVkJykKCgoKIyNzb3VyY2UgL2V0Yy96c2gvbmV3dXNlci56c2hyYy5y
    ZWNvbW1lbmRlZAojI3Byb21wdCBwd3MKIyMgT25seSBsb2FkIGNvbXBsZXRpb24gcGFydCBvZiB0
    aGUgZmlsZS4KI3NvdXJjZSA9KHNlZCAtbnJlICcvXmF1dG9sb2FkLipjb21waW5pdC8sJHAnICcv
    ZXRjL3pzaC9uZXd1c2VyLnpzaHJjLnJlY29tbWVuZGVkJykK
  )"
  local file_contents=''
  local char
  <<(base64 -d -i <<(printf '%s' "$file_contents_src")) \
    while read -k1 -ru0 char; do
      file_contents+="${char}"
    done

  declare -ga virtual_file_etc_zsh_zshrc_bev
  virtual_file_etc_zsh_zshrc_bev+="${file_contents}"

  local checksum="$(  >>(sed -nre's/^[[:space:]]*([^[:space:]]+).*$/\1/p') \
                    sha512sum \
                      <<(printf '%s' "$file_contents")
                  )"

  if [[ "${checksum}" != "${valid}" ]]; then
    1>&2 {
      printf $'%s: %s \'%s\': %s\\n' \
        "$0" \
        "Sorry, not sourcing virtual file" \
        "$source_filepath" \
        "the checksum doesn't match!"
      printf $'  Checksum: %s…\n' "${checksum[1,32]}"
      printf $'  Expected: %s…\n' "${valid[1,32]}"
    }
  else
    printf $'%s: file \'%s\' doesn\'t exist.\nVirtually sourcing after filtering \'%s\'.\n' \
      "$0" \
      "${source_filepath}" \
      'virtual_file_newuser_zshrc_recommended'
    source =($=source_filter <<(printf '%s' "$file_contents"))
  fi
fi















if ((!$+true))    { declare -gir $(echo true   )=$[1==1] ;}
if ((!$+false))   { declare -gir $(echo false  )=$[0!=0] ;}

if ((!$+success)) { declare -gir $(echo success)=$(true)$? ;}

# TODO: (fc builtin can help) addressing history by last digits.  Also try this
# as a contribution to zsh!
function h! {
	1>&2 {
		printf '%s: %s\n' "$0" 'Sorry, not yet implemented.'
	}
	return 1
}




function std-example-expanded {
	printf '%s\n' "$(echo -nE $(cat <<- 'EOM'
		() if {} $_ >>|cmd fc -ln "$HISTCMD" && local domain=example.com && cert=$domain.cert crt=$domain.crt && </dev/null openssl s_client -connect zubkoland.org:443 >>| $cert && sed -nre'/^-+BEGIN CERTIFICATE-+$/,/^-+END-CERTIFICATE-+$/p' <$cert >$crt
	EOM ))"
}

function ssl-certificate-example {
	printf '%s\n' "$(echo -nE $(cat <<- 'EOM'
		() if {} $_ >>|cmd fc -ln "$HISTCMD" && local domain=example.com && cert=$domain.cert crt=$domain.crt && </dev/null openssl s_client -connect zubkoland.org:443 >>| $cert && sed -nre'/^-+BEGIN CERTIFICATE-+$/,/^-+END-CERTIFICATE-+$/p' <$cert >$crt
	EOM ))"
}

: eval_stdout && E_$_() {
	printf '%s\n' $'local buffer char; while read -k1 -ru0 char; do buffer+="$char"; done; eval "$buffer"'
} &&
	declare -grx "$_=$(E_$_)"               $_ &&
	declare -grx "E_$_=eval eval ${(q-)$(E_$_)}" $_

function std-cmd {
	>>| std-cmd.out fc -ln "$HISTCMD"
}

function foo {
	echo 'echo test 1>&2'
	echo 'echo test2'
	echo 'sleep 2'
} > >($=E_eval_stdout)

function std-out {
	local out='std-out.out'
	if (( $# )) {
		"$@" >>| "$out"
	} else {
		cat <<- 'EOM'
			set -x
			pushln begin
			() {
				if egrep -qi '(^[[:space:]]*TRAPEXIT|EXIT[[:space:]]*$)' < <(trap); then
					1>&2 {
						printf '%s: %s\n' "$0" 'Sorry, trap-appending not yet supported!  Aborting to not override EXIT trap.'
					}
					return -2
				fi
				if [[ ! -o local_traps ]]; then
					1>&2 {
						printf '%s: %s\n' "$0" 'Sorry, not yet supported without local_traps!'
					}
					return -3
				fi

				local -i stdout
				exec {stdout}>&1
				pushln $stdout
				exec 1>> "$out"
				pushln done
			}
			case "$(getln -e)" in
				(begin)
					1>&2 {
						printf '%s: %s\n' "$0" 'An error occured with the shell function `std-out`.'
					}
					return 2
					;;
				(done)
					trap '() if {} $_ local -i orig_stdout='"$(getln -e)"' && {orig_stdout}>&-' EXIT
					getln -e >>/dev/null
					;;
				(*)
					printf '%s: %s\n' "$0" 'An error occured with the shell function `std-out` during evaluation.'
			esac
		EOM
	}
} > >($=E_eval_stdout)

#alias one-line {
#() if {} $_ >>|cmd fc -ln "$HISTCMD" && local domain=zubkoland.org && cert=$domain.cert crt=$domain.crt && </dev/null openssl s_client -connect zubkoland.org:443 >>| $cert && sed -nre'/^-+BEGIN CERTIFICATE-+$/,/^-+END-CERTIFICATE-+$/p' <$cert >$crt
#}









#function require-sed-scripts {
#	if [[ "${(t)sed-x}" != 'association' ]]; then
#		declare -gA sed
#
#		sed[quote_sed_def]='s@^[[:space:]]PRIM@@q;p'
#
#		sed[learn_hold_indentation]="$(head -n 1 <<- 'PRIM'
#			1{h;s@^([[:space:]]*).*$@\1:@;x}
#		PRIM )"
#
#		sed[prepend_hold_space]="$(sed -nre 's@^[[:space:]]PRIM@@q;p' <<- 'PRIM'
#			x
#			s@[:\]@\\\1
#		PRIM )"
#	fi
#}
#
#function sed-sed {
#	require-sed-scripts
#	if
#}
#
#
#
#
#() {
#local uc_argv0="$1" && shift && local -a uc_args && set -A $_ "$@"
#local unlock_challenge_dir='$HOME'
#
#function eval-to_unlock-challenge-args {
#	local hold_indentation='1{h;s@^([[:space:]]*).*$@\1:@;x}'
#	local prepend_hold_space='s@[:\]'
#	printf '%s\n' "$(sed -nre'1{h;s@^([[:space:]]*).*$@\1:@;x} -e 'x;G;h' -e'x;G;h;s@^([^:]*):\1@@;' -ee'' -ep <<- $'EOM'
#		local -a cat && : "set -A $_" && repeat 2 $_ call_opts user_opts && : 'local -a'
#		cat+='--'
#
#		for arg ("$@") $cat+="$arg" && if [[ "$arg" == "$cat[-1,-1]" ]] shift cat
#		EOM
#	)"
#}
#
#function unlock-challenge-init {
#	local -a cat && : "set -A $_" && repeat 2 $_ call_opts user_opts && : 'local -a'
#	cat+='--'
#
#	for arg ("$@") $cat+="$arg" && if [[ "$arg" == "$cat[-1,-1]" ]] shift cat
#
#	for i ("$@") call_opts+="$i" && if 
#}
#
#function unlock-challenge
#
#function unlock-challenge-1-access-shell {
#	unlock-challenge-msg 2 '%s\n' 'Where are we?'
#	unlock-challenge-init -- "$@"
#	local dir="$HOME/unlock-challenge/1/"
#	mkdir -p "$dir"
#	1>>| 
#	unlock-challenge-command --no-confirm $'sed -nreep <<< \'while :;: ;:; sed -nre\\\'s@[^[:alnum:]$]@\\\\\\\1@g; s@^@echo @\\\' -eep ;:; done\''
#}
#} "$0" "@"















# # Alias utilities.

# Invoke the given function with all arguments connected, i.e. with no
# separator.
function connect-args    {"${==argv[1]}" "${(j::)==argv[2,-1]}" ;}
function alias-connected {connect-args alias "$@"               ;}
alias    aliasc=alias-connected

# TODO: document
function alias-auto-args {alias "$1"="${==argv[2,-1]}";}
alias    aliasa=alias-auto-args

# TODO: document.
#
# TODO: support multiple alias definitions!
function alias-general {
  if (( $# <= 1 )) {
    alias "$@"
  } \
  else {
    if (( $# >= 2 )) {
      if [[ "$2" == '=' ]] { argv[2]=() ;}
    }

    alias-auto-args "$@"
  }
}
alias aliasg=alias-general






":" << '":"'
>>>#script_id bairyn_zshrc#<<<

# TODO: next thing to work on: SEARCH FOR CLIPBOARD!

local +x -hH -i null

local filepath="$(realpath -- "$0")"
local script_id

function qread {
} >/dev/null

function {
  if ! 2>/dev/null \
    [[ -f "${filepath}" && -r "${filepath}" &&
      < <(egrep -m2 '^>>>#([[:space:]]*script_id.*[[:space:]]*)#<<<$' "${filepath}" 2>/dev/null) {
        read -u 0 script_id && ! $(read -u 0)
      }
    ]] {
    unset filepath
    unset script_id
  } \
  else {
    declare -r filepath
    declare -r script_id=$(sed -r 's/[[:space:]]*script_id[[:space:]]*([^[:space:]].*)?$/\1/' <<< "${script_id}")
  }
}



# Print markdown when not sourced.
case "${zsh_eval_context}" { (file|filecode);;(*)
  {
    if ! &>/dev/null { which sensible-browser && which markdown }; then
      >&2 {
        echo '^'"$0"$': Sorry, couldn't find all dependencies…'
        echo '.'"$0"$': …for markdown browsing.'
        echo '.'"$0"$':'
        echo '.'"$0"$': This script requires `markdown` and…'
        echo '.'"$0"$': …`sensible-browser` when executed.'
        echo '.'"$0"$':'
        echo '$'"$0"$': Good luck.'
        exit -2
      }
    fi
  } always {
    if (( ${#filepath+1} + ${#script_id+1} < 2 )) {
      >&2 {
        echo '^'"$0"$': Couldn't identify the source of this script.'
        echo '.'"$0"$':'
        echo '.'"$0"$': Sorry, this script unfortunately doesn't yet support…'
        echo '.'"$0"$': …invocation outside being sourced or executed as a…'
        echo '.'"$0"$': file.'
        echo '.'"$0"$':
        echo '.'"$0"$': In the meantime, ensure all of the following are true:'
        echo '.'"$0"$': * Either the script is sourced or executed.  '
        echo '.'"$0"$':   (It thinks it's not being sourced, which is its…
        echo '.'"$0"$':    main purpose; documentation is otherwise…
        echo '.'"$0"$':     rendered.
        echo '.'"$0"$':   )'
        echo '.'"$0"$': * When not sourced, ensure all of the following:'
        echo '.'"$0"$':   * That `realpath` invoked with positional argument 0  '
        echo '.'"$0"$':     returns a normal and readable file.'
        echo '.'"$0"$':   * That the script contains a script_id pragma line,'
        echo '.'"$0"$':     containing a line comprising only the following'
        echo '.'"$0"$':     line:'
        echo '.'"$0"$':
        echo '.'"$0"$': I haven't yet finished implementing these checks…'
        echo '$'"$0"$': …automatically I'm afraid.'
        exit -2
      }
    }
  }

  env -u DISPLAY sensible-browser <(
    markdown - <(
      < "${filepath}" {
# TODO: support for flags!  Until then, hard-code the number of
# lines to skip.
        # Render only below `markdown` marker.
        #
        # Then, drop all '^# ' from lines when rendering.
        sed -nr -e $'
          # Proceed through three states:
          #   1. Start dropping every line until the regexp for
          #   markdown matches.
          #   2. Once the current line matches, drop the next 2
          #   lines (TODO: this shouldn\'t be hard-coded).
          #   3. Stop deleting every line.  Each will then
          #   proceed to be process by our next script.

          # A line has just been read; it is placed into a
          # buffer called "pattern space".
          #
          # We always jump unconditionally to `main`, so that we
          # can define conditional procedures or grouped
          # sequences of commands without automatically and
          # unconditionally executing them.
          : start {
            b main
          } b done

          : state-dropping {
            # If this line matches, continue; otherwise done.
            s/^>>>:[[:space:]]*markdown.*:<<<$/&/
            t :dropping-to-waiting
            b done; :dropping-to-waiting

            # Matches.  Transition to `waiting`; delete the next
            # two lines (TODO: hard-coded).
            x;c...\n;x
          } b done

          : state-waiting {
            x;s/^(.)./\1/;x
          } b done

          : state-printing {
            p
          } b done

          # Line processing always starts here.  We can jump to
          # any of the above methods if we want, conditionally
          # or unconditionally.
          : main {
            b route
          } done

          : route {
            x;s/^./&/;x
            b if-memory-empty

            : if-empty-memory {
            t then-empty-memory
            b else-empty-memory
              : then-empty-memory {
                # Empty buffer; state 1.
                b state-dropping
              } b endif-empty-memory
              : else-empty-memory {

                : if-single-memory {
                x;s/^.$/&/;x
                t then-single-memory
                b else-single-memory
                  : then-zero-memory {
                    # Singleton buffer; state 3.
                    b state-waiting
                  } b endif-single-memory
                    # Multi-character buffer; state 2.
                  : else-zero-memory {
                    b state-printing
                  } b endif-single-memory
                } : endif-single-memory

              } b endif-empty-memory
            } : endif-empty-memory
          } b done

          : done {
          }

            /^>>>:[[:space:]]*markdown/s/^//
        ' -e $'
          # Finally, simply strip \'# \' from the beginning of
          # lines, and wrap everything else in fenced code
          # blocks simply by adding 4 spaces.
          s/^#[[:space:]](.*)$/\1/
            t;s/^.*$/    \1//
      }
    )
  )
}

":" << '":"'
>>>:main:<<<
>>>:markdown -O 1 -re $'^":"$':<<<
":"



alias zsh-test="env ZDOTDIR=${HOME}/git/profiles/bairyn/skeletons/home-base -lis"
alias zsh-test="env ZDOTDIR=${HOME}/git/profiles/bairyn/skeletons/home-graphical -lis"
alias zsh-test="env ZDOTDIR=${HOME}/git/profiles/bairyn/skeletons/TODO -lis"

#emulate -LR zsh
emulate -R zsh


#echo 'TODO: ensure  which message  exists!' 1>&2
#echo 'TODO: better name for  /usr/local/bin/message!' 1>&2

# Colors
# man 5 terminfo
# HIERARCHICAL!
function help-control-charactors { TODO }
# \e[m (`alt-[` _code_ `m`)
#declare -g ESC_SEQ="\x1b["
#declare -g COL_RESET=$ESC_SEQ"39;49;00m"
#declare -g COL_RED=$ESC_SEQ"31;01m"
#declare -g COL_GREEN=$ESC_SEQ"32;01m"
#declare -g COL_YELLOW=$ESC_SEQ"33;01m"
#declare -g COL_BLUE=$ESC_SEQ"34;01m"
#declare -g COL_MAGENTA=$ESC_SEQ"35;01m"
#declare -g COL_CYAN=$ESC_SEQ"36;01m"


#alias which='which -a'
#alias whence='whence -a'
#alias type='type -a'
#alias where='type -a'
# Oh!  Just use "where"! :D


#ln -s home-base dot-files
# TODO: in ".zshrc", search for "quiet" in definition of "pop".  You were
# working on this!

# TODO: daemon to ensure lesskey is up to date!  lesskey -o >(sha512sum)






setopt interactivecomments  # Support comments on the command line.
#setopt inc_append_history
#setopt append_history
setopt always_last_prompt
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_name_dirs
setopt auto_param_keys
setopt auto_param_slash
setopt auto_pushd
setopt auto_remove_slash
setopt auto_resume
setopt correct
#setopt hist_ignore_dups
setopt hist_ignore_space
#setopt hist_no_store
setopt no_hist_no_store
setopt no_hup
##setopt no_beep
#setopt beep
setopt APPEND_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt rc_quotes




setopt extendedglob


eval set '-o '{correct,no_correct_all}
set -o no_menu_complete
set -o hup
set -o hash_list_all
set -o no_list_beep

set -o no_match NO_null_match

# You certainly want beep enabled in general, but this setting is *only* for
# *zle* errors, i.e., IIUC, errors generated from editing and typing in the
# command line.
set -o no_beep












set -o dvorak


set -o no_clobber

set -o no_share_history
set -o inc_append_history
set -o inc_append_history_time

#set -o complete_aliases

set -o multi_os

set -o extended_history
set -o hist_fcntl_lock
set -o no_hist_save_no_dups
set -o hist_expire_dups_first
set -o bang_hist
set -o no_hist_lex_words
set -o no_hist_reduce_blanks
set -o hist_save_by_copy
set -o no_hist_verify

set -o no_flow_control  # Disable ^S/^Q's effects.

set -o no_path_script
set -o mark_dirs

set -o no_hist_no_functions
set -o hist_reduce_blanks
set -o no_global_export
set -o no_all_export
##set -o no_rc_quotes
#set -o rc_quotes
set -o sh_word_split
set -o print_exit_value
set -o err_return
set -o warn_create_global
set -o no_rematch_pcre
set -o no_rc_expand_param

set -o correct
set -o no_correct_all

set -o no_auto_resume

set -o auto_menu -o no_bash_auto_list

# local VAR_BEGINNING_WITH_SLASH=/usr/bin
# cd VAR_BEGINNING_WITH_SLASH
set -o cdable_vars


# cdpaths -
#cdpaths() {
#	local -i endopts && let $_=$[false]
#	for arg in "$@"; do
#		if ((!$endopts)) && [[ "$arg" == "--help" || "$arg" == "--version" ]]; then
#			let endopts=$[true]
#		fi
#
#		if [[ "${arg[1]}" == '-' ]]; then
#	done
#}
#

cdpaths-clear() {
	declare -ga cdpaths
	cdpaths=()
	unset cdpaths
}
cdpaths-clear

cdpaths-reload() {
	declare -ga cdpaths
	cdpaths=('.')

	local cdpaths_dir
	if [[ -n "${XDG_CONFIG_HOME-}" ]]; then
		cdpaths_dir="${XDG_CONFIG_HOME}/cdpaths"
	elif false; then
		cdpaths_dir="$HOME/.cdpaths"
	else
		cdpaths_dir="${HOME}/.config/cdpaths"
	fi

	if ! [[ -d "${cdpaths_dir}" ]]; then
		() {
			setopt local_{options,traps,patterns} xtrace
			{
				set -x
				mkdir -p "${cdpaths_dir}"
			}
		}
	fi

	local file resolved
	while read -ru0 file; do
		if resolved=$(readlink -- "${file}"); then
			cdpaths+="${resolved}"
		else
			cdpaths+=($("${file}"))
		fi
	done < <( > >(sort) \
		cat \
			<(find "${cdpaths_dir}" -mindepth 1 -type l -xtype d) \
			<(find "${cdpaths_dir}" -mindepth 1 -type f -executable)
	)
}
cdpaths-reload

function mkcd {
	#. mkcd "$@"
	mkdir "$@" && cd -- "$_"
}

function chpwd() {
	timeout 0.1 lt
}

#for file in find "${XDG_CONFIG_HOME:-""}" do
#cdpath=()
#cdpath+='.'
#cdpath+='$HOME/git'
#cdpath+='$HOME/images'
#cdpath+='$HOME/references'
#cdpath+='$HOME/standards'
#cdpath+='$HOME/specifications'
#cdpath+='etc'
#cdpath+='$HOME'
#cdpath+=("$HOME"/git/*)







#set -o prompt_subst
bev-prompt






if [[ -n "${TMUX+y}${TMUX_+y}" ]]; then
  #TERM='screen-256color' && export TERM
  TERM='screen.rxvt' && export TERM
else
  #TERM='xterm-256color'  && export TERM
  TERM='rxvt-256color'  && export TERM
fi

# Thanks https://stackoverflow.com/a/22202558!

# Colored man pages: http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
# Less Colors for Man Pages
#export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
#export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
#export LESS_TERMCAP_me=$'\E[0m'           # end mode
#export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
#export LESS_TERMCAP_so=$'\E[38;5;016m\E[48;5;220m'    # begin standout-mode - info box
#export LESS_TERMCAP_ue=$'\E[0m'           # end underline
#export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;35m'       # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[01;32;7m'     # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# See terminfo(5) (and possibly the documentation of `terminfo`'s predecessor,
# termcap(5)) for more information on the topic.
#
# `Terminfo` is a database of terminal capabilities.  It contains especially
# which `ANSI escape codes` (c.f. [_ANSI escape
# codes_ on Wikipedia](https://en.wikipedia.org/wiki/ANSI_escape_code)) and
# other terminal-specific codes should be emitted for particular effects and
# colors by conformant applications, amongst other terminal descriptions.

# I obtained the usages of each of these `terminfo` variables by `less`
# experimentally.

# TODO: document!
# `TODO: how is this used by less`?
#  * `mb`: enter_blink_mode
#  * `mb`: enter_bold_mode
export LESS_TERMCAP_mb=$'\E[01;31m'       # TODO: start: TODO:<how is this used by less?> (blinking)
export LESS_TERMCAP_md=$'\E[01;35m'       # TODO: headers, key-words (bold)  # (orig: begin bold)
export LESS_TERMCAP_me=$'\E[0m'           # TODO: end   bold                       # (orig: end mode)
export LESS_TERMCAP_se=$'\E[0m'           # TODO: (orig: end standout-mode)
#export LESS_TERMCAP_so=$'\E[01;32;7m'     # TODO: (orig: begin standout-mode - info box)
#export LESS_TERMCAP_so=$'\e[7m\e[34m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_ALT0CAP_so=$'\e[07;34m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_ALT1CAP_so=$'\e[01;35;07m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_ALT2CAP_so=$'\e[01;35;07m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_ALT3CAP_so=$'\e[01;31;07m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_TERMCAP_so=$'\e[01;31;07m'     # start: search highlighting: (orig: begin standout-mode - info box)
export LESS_TERMCAP_ue=$'\E[0m'           # TODO: (orig: end underline)
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # TODO: (orig: begin underline)

# \E[01;32;7m

alias q=exit







# Usage: $=require=$ arguments.
#
# It also provides a general shushed pragma, $=shushed=$, 
#
# Also: $=shushed=$ require foo
# shushed is a general method for calling.

## define-pragma name 
#function define-pragma {
#}






## Quoting with parentheses.



: $_ = "
  $(cat<<-$'\n'
    b _main

    :_main
      x;s/\`\'/&/;x;t _initialize
      x;s/\`/

    :_initialize

    :route
      :if   x;s/^$/&/;x
      :then b initialize

      :if   x;s/^$/&/;x
      :then b initialize
    :initialize
      x;s/.*//;x

    :state-default
    :state-
      x;s/^/empty:/;x
    :b b route-end

    :call
      x;s/^.*/

  )
"
: $_ = ${==:-"sed -nre'$_'"//$'\n'/;}
#alias -g paran-quote="$($_ <<- \$'\\n'\n"

#alias -g paran-quote=paran-quote-raw

#alias -g paran-quote=paran-quote-raw-

#alias -g paran-quote=paran-quote-expand

alias help-paran-quote=paran-quote_synopsis
function paran-quote_synopsis { }



















#declare -gax REQUIRE_META="${REQUIRE_META-$(<<-$'\n'
#
#
#
#
#
#
#
#
#
#
#$REQUIRE_META v0.1$
#
#
#$(;: META :;)
#()<<PRAGMA : --version 0.1 --name 'man-sticky-storage'>>()
#PRAGMA storage man-sticky-options
#PRAGMA storage man-sticky
#PRAGMA
#
#
#
#: META {
#
#
#()<<}': PRAGMA :'>>()
#()>>>STICKY>>>()
#STICKY:MANOPTS:
#MANOPTS
#
#()<<END BLOCK>>():
#PRAGMA


#declare -g -x MANOPT='-a'
#declare -g -x MANOPT='-a --regex'
declare -g -x MAN_KEEP_STDERR='1'

## Print all arguments until a non-option parameter, or the end, is encountered.
#function print-while-option {
#  while [[ "${${1:-x}[1]}" == '-' ]] { echo -e "$1"; shift }
#}
#
#function man-functions_synopsis {
#  echo -ne $'man API for man options ($MANOPT).  Main:\n'
#  echo -ne $'  * mand (defaults): no MANOPTS\n'
#  echo -ne $'  * manf (fixed): MANOPTS w/opt --regexp\n'
#  echo -ne $'  * mans: \n'
#}
#
#function mand_defaults_synopsis { echo 'man: Default flags; do''nt forward $MAN_OPT.' ;}
#function mand {
#  env -u MANOPT man "$@"
#}
#
#function manc_current_synopsis { echo 'man: Use current $MANOPT flags (trivial wrapper).' ;}
#function manc {
#  man "$@"
#}
#
#{
#  function man-current-get_synopsis { echo 'Prints $MANOPT.  Does not invoke man.' ;}
#  function man-current-get {
#  }
#
#  function man-current-add_synopsis { echo 'Appends to $MANOPT.  Supports +.  Removes duplicates.  Does not invoke man.' ;}
#  function man-current-add {
#  }
#
#  function man-current-set_synopsis { echo 'Sets $MANOPT.  Does not invoke man.' ;}
#  function man-current-set {
#  }
#}
#
#function mans_sticky_synopsis { echo 'man: Set MANOPT' ;}
#function mans {
#  MANOPT="$(print-while-option )"
#  env MANOPT="$(<<< "$MANOPT" sed -nre's/--regex//g;p;q')"
#}
#
#function manf_synopsis { echo 'man: Fixed-string lookup, all matches.' ;}
#function manf {
#  env MANOPT="$(<<< "$MANOPT" sed -nre's/--regex//g;p;q')"
#}


#alias l='lt +A'


#alias vim='vim -Nu ${HOME}/.vimrc.orig -p'
alias vim='vim -N -p'



function date-fmt { date "${argv[@]}" +'%Y-%m-%d_%H:%M:%S' ;}
function now      { date "${argv[@]}" +'%Y-%m-%d_%H:%M:%S' ;}
alias n=now

export GREP_OPTIONS="--color=auto"



cat << 'end-doc' >> /dev/null
% ulimit -a
-t: cpu time (seconds)              unlimited
-f: file size (blocks)              unlimited
-d: data seg size (kbytes)          unlimited
-s: stack size (kbytes)             8192
-c: core file size (blocks)         0
-m: resident set size (kbytes)      unlimited
-u: processes                       15332
-n: file descriptors                65536
-l: locked-in-memory size (kbytes)  64
-v: address space (kbytes)          unlimited
-x: file locks                      unlimited
-i: pending signals                 15332
-q: bytes in POSIX msg queues       819200
-e: max nice                        0
-r: max rt priority                 0
-N 15:                              unlimited
end-doc

#ulimit 

declare -x -i REPORTTIME=5; 
export REPORTTIME=5





# Introduce new scope local to the next command.
alias cmd-scope='function() '

# Introduce a local variable in scope only to the following command.  Can be
# composed via juxtaposition.
alias local-cmd='function() local'




#set -o ignore_braces ignore_close_braces


# The trick is ${ $(:)+

# echo ><{ hello world! }><
#alias -g '><{'='{${$(:)+'
#alias -g '><'='}'        
#
#alias '><{

declare -g _zero_hash
:; {
  declare -A  _zero_hash
  _zero_hash[0]='0'
  declare -gr _zero_hash
}


# Prints 0 if last result is 0; except if args are passed, set $0 to what would
# otherwise be printed, and then call them instead.
cmd-scope {
  local scope_zero_hash

  :; {
    declare -A  scope_zero_hash
    scope_zero_hash[0]='0'
    declare -gr scope_zero_hash
  }

  function printly-result {
    local -i result=$?

    local    printly="${scope_zero_hash[$result]}"

    if (( $# <= 0 )) {
      echo "${printly}"
    } \
    else {
      0="${printly}" "${=argv[@]}"
    }
  }
}
alias -g '$??'='$_zero_hash[$?]'



function set-local-from-alias {
}
alias -s 'local'='set-local-from-alias'



function strip-leading-trailing-whitespace {
  sed -r 's/^[[:space:]]*(.*)[[:space:]]*$/\1/' <<< "${=argv[@]}"
}




alias -g '_apply-alias-as-global'=' '

alias 'start-quote'="\${==\${(e)==\$(:)+eval strip-leading-trailing-whitespace \${==\$(:)+"
alias -g '{{{'='_apply-alias-as-global start-quote'



# echo "${${foo+set to $foo}:-${foo-not set}}"




function debug-args { echo "$0 ($#): ${(q-)==argv[@]}" }


local-cmd  alias q='function { if (( $# <= 0 )) { exit 0  } { "$@" } }'



function line { local l; read -r l; echo "$l" }

function consume-input { local null; while { read -t -ru0 null } { }; while { read -k1 -t -ru0 null } { } }

function sub-shell {
	subshell "$@"
}
function subshell {
	local shell="${SHELL:-"${$(realpath -- "$(=which -- 'zsh')"):-"${$(realpath -- "$(=which -- '/bin/zsh')"):-'/bin/sh'}"}"}"
	local -a args && set -A "$_" "-${--il}"

	if { { [[ "${1-x}" == '--' ]] && shift ;} || [[ "${1[1]--}" != '-' ]] ;}; then
		shell="$1"
		shift
	fi

	if (( $# )); then
		set -A args "$@"
	fi

	"$shell" "${args[@]}"
}

function println {
	if (( $# )) 1+='\n'
	printf "$@"
}

set -o multi_func_def
function {eval,EVAL}-{,expn-}{start,end} {eval,EVAL}-{,no,no-}expn {
	1>&2 {
		printf     '%s: traditional heredoc delimiter name used as shell function invocation!  This is probably a mistake.\n' "$0"
		printf     '%s: Arguments:\n' "$0"
		printf "$0"': * %q\n' "$@"
		printf     '%s: Aborting.'
		return 1
	}
}

## Each function respects other unctions defined via this prim.
##
## Sets "args" to all arguments after the initial
#function define-precmd-function {
#	local -A precmds; set -A precmds
#
#	local define_precmd_function="$(cat)" <<- eval-start <<- 'eval-end define_precmd_function'
#	eval-start
#		local definition="$(cat)"
#		local -i escaped=0
#		if [ "${1-x}" = '--' ]; then
#			shift
#			escaped=1
#		fi
#
#		if (( !$# )) return 0
#		local name="$1"; shift
#
#		if (( !$escaped )) && [ "${name[1,2]-x}" = '--' ]; then
#			case "name" in
#				(version)
#					1>&2 {
#						printf '$0: Sorry, this option is not yet implemented!: %s\n' "$0" "$name"
#						return -3
#					}
#					;;
#				(help)
#					1>&2 {
#						printf '$0: Usage: %s [--help|--version] [name|flag]… [--] [name]… < definition\n' "$0" "${(q-)0}"
#						printf 'TODO: adds cmds array and args array.\n'
#						return -2
#					}
#					;;
#				(*)
#					1>&2 {
#						printf '$0: Sorry, unrecognized option, so I'\''ll abort: %s\n' "$0" "$name"
#						return -1
#					}
#					;;
#			esac
#		fi
#
#		precmds+=("$name" "$definition")
#
#		eval "$(cat)" <<- 'EVAL-start' <<- 'EVAL-expn-start' <<- EVAL-expn-end <<- 'EVAL-expn-start' <<- EVAL-expn-end <<- 'EVAL-end auto_define-precmd-function'
#		EVAL-start
#		EVAL-expn-start
#			function define-precmd-function {
#				local -A precmds; set -A precmds "${(kvq-)precmds[@]}"
#
#		EVAL-expn-end
#				local define_precmd_function="$(cat)" <<- eval-start <<- 'eval-end define_precmd_function'
#		EVAL-expn-start
#				eval-start
#					"$define_precmd_function"
#				eval-end
#		EVAL-expn-start
#			}
#		EVAL-end auto_define-precmd-function
#
#		if (( $# )); then
#			define-precmd-function  "$@"
#		else
#			for cmd def in "${(kv)precmds[@]}"; do
#				eval "$(cat)" <<- EVAL-start <<- 'EVAL-expn-start' <<- EVAL-expn-end <<- 'EVAL-end func'
#				EVAL-start
#				EVAL-expn-start
#					function "$cmd" {
#						local -a cmds args
#
#						set -A cmds "$[${#precmds}+1]" "${(k)precmds[@]}"
#				EVAL-expn-end
#						set -A args "$@"
#
#						if (( ${#cmds} >= 2 )); then
#							while (( ${#args} && ${cmds[2,$cmds[1]][(I)$args]} )); do
#								cmds+="$args[1]"
#								shift args
#							done
#						fi
#
#						shift cmds "${cmds[1]}"
#
#						"$def"
#					}
#				EVAL-end func
#			done
#		fi
#	eval-end header
#}

# precmd
#function ln {
#	if (( $# >= 2 ))
#}

function while-confirmed {
	local -i noprompt_num=0
	local -a last_return_status

	if   [ "${1-x}" = '--' ]; then
		shift
	elif [ "${1-x}" = '--help' ]; then
		1>&2 {
			printf 'Usage: %s [SKIP_NUM=0\n' "${(q-)0}"
			printf '\n'
			printf '* SKIP_NUM:\n'
			printf '* SKIP_NUM:\n'
			return 2
		}
	elif (( $# >= 1 )); then
		if is-integers "$1"; then
			noprompt_num="$1"
			shift
		fi
	fi

	if   [ "${1-x}" = '--' ]; then
		shift
	elif (( $# )); then
		if [ "${1-x}"
		if is-integers "$1"; then
			noprompt_num="$1"
			shift
		fi
	fi

	#while { consume-input && { (( noprompt_num-- >= 1 )) || echo -n 'New subshell? [y/N] '; read -r l && [[ "${l:-n:l}" == "y" ]] } } {
	while
		#(( noprompt_num < 0 || ( noprompt_num > 0 && noprompt_num-- ) )) || {
		(( noprompt_num <= 0 ? noprompt_num : noprompt_num-- )) || {
			1>&2 {
				printf '[%s] New %sshell? [y/N] ' "$0" "$(repeat $[SHLVL] printf 'sub')"

				#local char
				#local line
				#consume-input
				#while read -k1 -r char; do
				#	line+="$char"
				#	if [[ "$char" == $'\n' ]]; then break; fi
				#done
				#
				#if [[ "${line[$]}" != $'\n' ]]; then
				#	printf '\n'
				#fi
				#
				#[[ "${line:-n}" == 'y' ]] || [[ "${line:-n}" == $'y\n' ]]

				local -t line
				consume-input
				#if ! read -r -s line; then
				if ! read -r line; then
					printf '\n'
				fi
			}

			[[ "${${line:-n}:l}" == 'y' ]]
		}
	do
		local -i return_status=0

		sub-shell "$@" || return_status=$?
		set -A last_return_status "${return_status}"

		if (( $return_status != 0 )) {
			1>&2 {
				printf '%s: Sushell returned %d.\n' "$0" "$return_status"
				printf '%s: Aborting.\n' "$0"

				break
			}
		}
	done

	if (( !$#last_return_status )); then
		1>&2 {
			printf '%s: (Remaining on %sparent shell%s.)\n' "$0" "$(if ((SHLVL <= 1)) printf '\e[1mtop-level\e[0m ')" "$(repeat $[SHLVL-1] printf '_')"
		}
	else
		1>&2 {
			printf '%s: Back to %sparent shell%s.\n' "$0" "$(if ((SHLVL <= 1)) printf '\e[1mtop-level\e[0m ')" "$(repeat $[SHLVL-1] printf '_')"
		}
	fi
}
alias 'sub-shells'='while-confirmed'
alias 'zsh-sub-loop'='sub-shells'

alias 'debug-zsh-sub-loop'='zsh-sub-loop'

alias 'zsh-sub-loop-inf'='zsh-sub-loop -1 '
alias 'debug-zsh-sub-loop-inf'='zsh-sub-loop -1 '

alias 'sub-loop'='zsh-sub-loop'
alias 'subshell-loop'='zsh-sub-loop'
alias 'sub-shell-loop'='zsh-sub-loop'

#function foo { echo 'echo $argv'; cat } <&0 > >(local l;while read l;do eval "$l"; done); foo < <(echo echo test)

# moved above
#set -o multi_os

alias swap-stdout-stderr='exec {swap}>&1; exec 1>&2 exec 2>&$swap exec {swap}>&-; echo uh oh "$@"'

# Define a function that can call 'super_eval'.
alias function-eval=$'function {
  local -i output input
  exec {output}>/dev/null {input}</dev/zero
  print -z "$input" "$output")
function { local -i output input; output } && '

function super_eval {
}

# Some tricks that may help:
#
# > >(eval $(cat)) function { local -i eval; exec {eval}> >(local l; while { read -r l } { eval "$l" }); return $eval }; e=$?
#
# Use "print -z" and "read -z".


# Define a function whose standard output is evaluated.
#
# Inside a "function-eval", each command must be prefixed with one of the
# following:
#
# "stdout": 
alias function-eval='eval-stdout function'

cat << 'COMMENT-foo' >> /dev/null
alias $(echo eval-stdout       )="$(echo -nE \
  '> >(local l; while { read -r l }            { eval "$l" })')"
alias $(echo eval-stdout-single)="$(echo -nE \
  '> >(eval $(cat))'                                          )"
alias $(echo eval-stdout-no-raw)="$(echo -nE \
  '> >(local l; while { read l }               { eval "$l" })')"
alias $(echo eval-stdout-null  )="$(echo -nE \
  '> >(local l; while { read -r -d '$'\0'' l } { eval "$l" })')"
COMMENT-foo

alias $(echo eval-stdout       )="$(echo -nE \
  '> >(local l; while { read -r l }            { eval "$l" })')"

alias assign-til-empty-line="$(echo -nE $(echo -nE      \
  $'> >( local var;'                                    \
  $'     local l;'                                      \
  $''                                                   \
  $'     while { read -r l }'                           \
  $'     {'                                             \
  $'       local -a words; words=($l);'                 \
  $'       if (( ${#words} >= 1 ))'                     \
  $'       {'                                           \
  $'         var="${words[1]}";'                        \
  $'         local -i -r start=$(( ${#var} + 1 ));'     \
  $'         eval "${(q-)var}+=${(q-)l[${start},-1]}";' \
  $'         break;'                                    \
  $'       }'                                           \
  $'     }'                                             \
  $''                                                   \
  $'     while { read -r l }'                           \
  $'     {'                                             \
  $'       eval "${(q-)var}+=\\${(q-)l}";'              \
  $'     }'                                             \
  $'   )'                                               \
  $'cat <<- \'\''                                       \
  ))"$'\n'

# TODO: example
alias    function-argv='feed-argv <&0 function-eval'
# Sets $this_argv and $super_argv
alias    function-argv-init='local -a this_argv; this_argv=( "${argv[@]}" ); read-super_argv;'
function write-super_argv { write-super_argv-from "${super_argv[@]}" }

alias    read-super_argv='local -a super_argv; read-super-argv-into super_argv'

# `ee` can be used to emit output inside `function-argv` and other functions
# based on `function-eval`.
alias    ee='echo echo'

alias    read-super_argv-into='read -r -A'
function write-super_argv-from { echo "argv=(\"${(q-)@}\")" }

alias feed-argv=$'< <(eval \'echo "${argv[@]}"\')'








cat << 'COMMENTaoeu' >> /dev/null


# copy to "primary" clipboard
{
  declare -g -r default_clipboard_id='primary'

  declare -A -g clipboards
  clipboards=\
    ( 1    primary
    , 8    primary
    , '*'  primary

    , 1    primary
    )
  declare -r    clipboards

  function get-clipboard {
    snth
  }
}


# "copy _clipboard_" prefixes the command
function-eval copy {
  local -A this_argv
  this_argv=("${argv[@]}")

  local clipboard="${this_argv[(e)${1-${default_clipboard_id}}]}"
  echo ''

  echo "${this_argv[@]}"
}

alias $(echo copy          )='> >()'
alias $(echo copy-primary  )='> >(xset -o -selection primary)'
alias $(echo copy-clipboard)='> >(xset -o -selection primary)'



COMMENTaoeu

















alias is-set='test '
function-eval is-set


function id     { "${argv[@]}" }
alias    pre-cmd='if [[ -n "${this_argv[@]+1}"  ]] { "${this_argv[@]}" } else { "${argv[@]}" }'

# Use more complicated input to eval for both evaluation and printing to
# standard output.
function to-stdout { id } > >(local l; while { read -r l } { echo -E "echo -E ${(q-)l}" })
alias eval-echo-to-stdout='echo echo'

function is-digits {
  for arg ("${argv[@]}") { egrep -q -e '^[[:digit:]]$' <<< "${arg}" || return 1 }
  return 0
}

function is-integers {
  for arg ("${argv[@]}") { egrep -q -e '^[-+]*[[:digit:]]$' <<< "${arg}" || return 1 }
  return 0
}

## pop [n=1] array_name [variable_to_set_0 [to_set_1 [to_set_... [to_set_n  [array_name_2 [variable_to_set_0 [...]]]]]]]
# Can also do:
#   read-for-pop pop 3 foo bar
# pop [n=1] array_name [array_name_2 [array_name_3 [...]]]
function-argv rpop {
  function-argv-init

  local -i num_to_pop=1

  if is-digits "${this_argv[1]}"; then
    num_to_pop=${this_argv[1]}
    this_argv=("${this_argv[2,-1]}")
  fi

  local -i -r new_index=$(( ${num_to_pop} + 1 ))

  if (( $# <= 0 )); then
    ee "${super_argv[1,${num_to_pop}]}"
    super_argv=("${this_argv[${new_index},-1]}"); write-super_argv
  else
    for array_name ("${this_argv[@]}") {
      ee "${(P)${array_name[@]}[1,${num_to_pop},]}"
      echo "${(q-)array_name}=${(P)${array_name[@]}[{new_index},-1]}"
    }
  fi
}

# Use *before* a call to pop!
#alias read-for-pop='> >('
# TODO



alias    silence-cmd=silence-stdout
function silence-cmd-stdout { id } 1> /dev/null
function silence-cmd-stderr { id } 2> /dev/null
function silence-cmd-both   { id } &> /dev/null

alias $(echo silence       )='silence-stdout'
alias $(echo silence-stdout)='1> /dev/null'
alias $(echo silence-stderr)='2> /dev/null'
alias $(echo silence-both  )='&> /dev/null'


# Booleans.
#declare -r -i -g true=$(( 1 == 1 ))  # Already defined above.
#declare -r -i -g false=$(( !$true ))  # Already defined above.


# Return statuses.

# Pass a return status as an argument, else default to $? as the status.
function is-status-success { echo "$(( $success == ${1-$(head -n1)} ))" }

# Invoke the provided command, printing the returned status to standard output.
function cmd-status  { echo "$($argv)$?" }
# Invoke the provided command, printing a *boolean* indicating whether it was
# successful.
function cmd-success { is-status-success "$(cmd-status "$@")" }

#function-super-argv pop


# TODO:
#
#  - if-bool 1           'foo'      'bar'
#  - if-bool $true       'foo'      'bar'
#  - if-bool 'echo true' 'echo foo' 'echo bar'
#
#  - if-bool _            _          _          'echo'
#  - if-bool _            _          _          'eval'
#
#  - if-bool _            _          _          echo always-first
#  - if-bool _            _          _          leval 'echo "The value is: $1."'
#
# TODO: warn about difference between bool and  return status (see below)
function if-bool {
}

# A wrapper around "if-bool" that treats parseable integers passed as the first
# argument as status codes, rather than booleans.  These are generally encoded
# incompatibly reversely!
function if-status {
}

cat << 'COMMENT-conditionals' >> /dev/null

# Takes three arguments: a boolean, e.g. one obtained from "cmd-success", and
# two strings.  The first is printed if the boolean is true, otherwise the
# second is.  The third defaults to the output of "print_false", and the second defaults to
# that of "print_true".  The first argument defaults to "$?"  These are printed
# with 'echo'.
#
# Any further arguments are provided, 'echo' is replaced with these commands
# string to evaluating if true, and a string to evaluate if false.
function if-success {
  local -i result=$?
  local -a args; args=($argv)

  local -i success=${${argv[1]}-$?}
  local 'then'="${${argv[2]}-$(print-true)}"
}

# A version of "if-success" that defaults to "eval" rather than "echo" as its
# command.
#
# Additionally, if-success-eval's behaviour on the first argument, if provided,
# depends on the form of the argument: if it can be parsed as a string, it's
# interpreted, as usual, as a status code (*NOT* a boolean, typically encoded
# incompatibly reversibly); otherwise, it is evaluated at the meta-level to
# obtain the object-level status result
function if-success-eval {
  TODO
}

# A version of "if-success-eval" that evaluates the first argument if provided
# to obtain its return status, rather than treating it as a status at the
# object level.
function if-eval { TODO }

function if

COMMENT-conditionals

# Wrapper around 'eval', printing either $true or $false.
function eval-succeeds { if { eval "$@" } { print-true } else { print-false } }

declare -i -g "$(echo initial_interactive)=$(( 0 == $(fgrep -q -e 'i' - <<< "$-")$? ))"
declare -i -g "$(echo initial_login      )=$(( 0 == $(fgrep -q -e 'l' - <<< "$-")$? ))"


if (( ${initial_interactive} )) { set -o warn_create_global }


# If false, alias to the "del-perm" *function*, a wrapper around "rm", instead.
declare -ir -g rm_trash_alias=$true

# Wrapper around "rm" that respects 'rm's flags.
function del {
  # TODO: actually respect "rm"'s flags.
  trash-put "$@"
}

function del-perm {
  "$(command which rm)" --one-file-system  "$@"
}

if (( ${initial_interactive} )); then
  if (( ${rm_trash_alias} )); then
    alias rm=del
  else
    alias rm=del-perm
  fi
fi

alias jctl='journalctl -x --lines -o verbose'
alias jctl-rt='jctl --follow'  # rt: realtime

alias jctl-sys='jctl --system'
alias jctl-sys-rt='jctl-rt --system'

alias jctl-user='jctl --user'
alias jctl-user='jctl-rt --user'








#alias tmux='tmux -2u'
alias tmux='tmux -u'

## temporarily identify your outer shell
##export PROMPT="%i - %T - %n@%m:%~%# "
##export PROMPT="outer - %i %T %n@%m:%~%# "
#export PROMPT="%i %D{%H:%M:%S.%.} %n@%m:%~%# "

# Lines configured by zsh-newuser-install
#setopt all_export
HISTFILE=~/.zhistfile
SAVEHISTFILE=~/.zhistfile
HISTSIZE=1000000
SAVEHIST=1000000
#HISTSIZE=1024
#SIZEHIST=9999999
## moved above
#setopt interactivecomments  # Support comments on the command line.
##setopt inc_append_history
##setopt append_history
#setopt always_last_prompt
#setopt auto_cd
#setopt auto_list
#setopt auto_menu
#setopt auto_name_dirs
#setopt auto_param_keys
#setopt auto_param_slash
#setopt auto_pushd
#setopt auto_remove_slash
#setopt auto_resume
#setopt correct
##setopt hist_ignore_dups
#setopt hist_ignore_space
#setopt hist_no_store
#setopt no_hup
##setopt no_beep
#setopt beep
#setopt APPEND_HISTORY
#setopt HIST_EXPIRE_DUPS_FIRST
#setopt HIST_FCNTL_LOCK
#setopt rc_quotes


bindkey -v
bindkey -M viins '^?'  backward-delete-char
bindkey -M viins '^H'  backward-delete-char

bindkey -M vicmd 'd'  vi-backward-char
bindkey -M vicmd 'h'  vi-down-line-or-history
bindkey -M vicmd 't'  vi-up-line-or-history
bindkey -M vicmd 'n'  vi-forward-char
bindkey -M vicmd 'j'  vi-delete
bindkey -M vicmd 'J'  vi-kill-eol
bindkey -M vicmd 'K'  vi-join
bindkey -M vicmd 'l'  vi-repeat-search

bindkey -M vicmd 'u'  undo
bindkey -M vicmd '^R' redo
bindkey -M vicmd 'U'  vi-undo-change

bindkey -M viins '^[[Z' undo

# TODO: figure out how to generate the below "^@" control character in vim (is
# it with ctrl-o or something?)?  Then document in a comment how to generate
# this, noting that this character can be restored by pressing typing this
# sequence in vim, and then change the actual binding to ASCII "^@".  Why?
# Because some tools, eg grep, diff, and git diff, incorrectly identify this
# configuration file, .zshrc, as a binary file rather than a text file.
# But I don't want to change the " " below to "^@" until I document how I even
# wrote that character *in vim* in the first place.  Also note there's another
# occurrence of this non-printable character in a comment shortly below, and
# *this* paragraph also introduces one that can go away when the actual
# documentation is written.  It'd be nice to not have to explicitly state that
# this file is a text file to git diff, git show, grep, diff, etc.
# Aha, I also found a control character in the definition of chpwd() shortly
# below.
bindkey -M viins ' ' vi-cmd-mode
#bindkey -M vicmd 'h' vi-backward-char
#bindkey -M vicmd 't' vi-down-line-or-history
#bindkey -M vicmd 'n' vi-up-line-or-history
#bindkey -M vicmd 's' vi-forward-char
#bindkey -M vicmd 'd' vi-delete
#bindkey -M vicmd 'D' vi-kill-eol
#bindkey -M vicmd 'K' vi-join
#bindkey -M vicmd 'l' vi-repeat-search
#bindkey -M viins ' ' vi-cmd-mode
#eval `ssh-agent -t 3600`  # One hour
export EDITOR=vim
export VISUAL=vim
# moved above
#setopt extendedglob
#source ~/.zsh_prompt
# End of lines configured by zsh-newuser-install
##export PROMPT="%i -%t - %n@%m:%c%# "  # shorter DIR
#export PROMPT="%i - %T - %n@%m:%~%# "
#PATH=~/.cabal/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#if [ -d ~/bin ]; then
#PATH=~/bin:${PATH} && export PATH
#fi
#alias lt="lt --color"
chpwd()
{
  print -Pn ']2;%~'
}
MAIL=/var/mail/bairyn && export MAIL
MAILDIR=/var/mail/bairyn && export MAIL

alias process='if [[ -z "$PROG" || -z "$FILES" ]]; then echo -ne "aborting: PROG or FILES is empty or not set\n" >& 2; false; else for FILE in $FILES; do cp -i $FILE .${FILE}.pbak; TMP=$(mktemp); ${=PROG} < $FILE > $TMP || echo -ne "Failed to process ${FILE}\n\n" && mv $TMP $FILE; done; unset PROG FILES; fi' # if user denies backing up file, it will still be overwritten unless the user cancels with ^C
alias processq='if [[ -z "$PROG" || -z "$FILES" ]]; then echo -ne "aborting: PROG or FILES is empty or not set\n" >& 2; false; else for FILE in $FILES; do cp $FILE .${FILE}.pbak; TMP=$(mktemp); ${=PROG} < $FILE > $TMP || echo -ne "Failed to process ${FILE}\n\n" && mv $TMP $FILE; done; unset PROG FILES; fi' # ALWAYS overwrite bak file if it exists
alias bell='echo -ne "\a"'
alias screenshot="scrot '%Y-%m-%d--%H:%M%S_\$wx\$h.png' -e 'mv \$f ~/screenshots/'"
export SDL_AUDIODRIVER="dsp"

#export LD_PRELOAD="${LD_PRELOAD}, /usr/lib/libtrash.so"
#export LD_PRELOAD="/usr/lib/libtrash.so"
#export LD_PRELOAD="/usr/lib/libtrash.so $LD_PRELOAD"

### ## ssh-agent BEGIN
### test=`/bin/ps -ef | /bin/grep ssh-agent | /bin/grep -v grep  | /usr/bin/awk '{print $2}' | xargs`
### 
### if [ "$test" = "" ]; then
   ### # there is no agent running
   ### if [ -e "$HOME/agent.sh" ]; then
      ### # remove the old file
      ### /bin/rm -f $HOME/agent.sh
   ### fi;
   ### # start a new agent
### #/usr/bin/ssh-agent | /usr/bin/grep -v echo >&$HOME/agent.sh
   ### /usr/bin/ssh-agent | /bin/grep -v echo >&$HOME/agent.sh
### fi;
### 
### test -e $HOME/agent.sh && source $HOME/agent.sh
### 
### alias kagent="kill -9 $SSH_AGENT_PID"
### ## ssh-agent END

## for tmux: export 256color
#[ -n "$TMUX" ] && export TERM=screen-256color

#alias mplayercd='mplayer -cdrom-device /dev/sg1 -cache 15000 -cache-min 80 cdda://'
#alias mplayercdnoprefix='mplayer -cdrom-device /dev/sg1'

#export LD_PRELOAD="/usr/lib/libtrash.so $LD_PRELOAD"
#alias ls='ls --color=auto'
#alias lt='ls'
#alias l='ls'
#export PATH=~/bin:~/.cabal/bin:$PATH
#PATH=~/bin:~/.cabal/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#PATH=~/bin:~/.cabal/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
#PATH=~/bin:~/.cabal/bin:~/.gem/ruby/2.0.0/bin:~/.gem/ruby/1.9.3/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH
PATH=~/bin:.cabal-sandbox/bin:~/.cabal/bin:~/.gem/ruby/1.9.1/bin:/usr/local/bin:/opt/java/bin:${PATH} && export PATH

#eval `ssh-agent`
##export TERM=xterm
#export TERM=xterm-256color
##export TERM=screen-256color

#wmname LG3D

#alias agda-wl="agda -i ./ -i ~/agda-lib"
#alias agda-wl="cat ~/agda2/opts | xargs agda -i ./ -i ./src -i ~/agda-lib"
#alias agda-wl="cat ~/agda2/opts | xargs ~/.cabal/bin/agda -i ./ -i ./src"

alias cls="echo -ne '\x1Bc'"
alias cl=cls
alias racket-wl="racket -il readline"
#alias rsync="/usr/bin/rsync -xravzP"

function rsync-old-compress {
  rsync -xravzP --old-compress "$@" --old-compress
}
alias rsync=rsync-old-compress

# Handles zero arguments.
function printf-proper {
if { (( $# >= 2 )) || [[ "$(printf "$1" 1)" == "$(printf "$1")" ]] } { printf "$@" }
}
alias p=printf-proper

function love {
  local heart=$'\e[1;31m❤\e[0m'
  local namefmt=$'\e[5m\e[35m%s\e[0m'

  p "$heart"
  p " $namefmt $heart" "$@"
  p '\n'
}

# XCompose for some apps.
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
#export LANG=en_GB.UTF-8
#export LANGUAGE=en_GB.UTF-8

#alias git0="unset GIT_WORK_TREE GIT_DIR"
#alias git1="export GIT_WORK_TREE=. GIT_DIR=.git_"
#alias git2="export GIT_WORK_TREE=. GIT_DIR=.git_"

alias v="vim -R"

alias mv="mv -i"
alias cp="cp -i"


alias fix-gtk="exec sudo -su bairyn"
alias fix-java="wmname LG3D"
#export JAVA_HOME=/usr/lib/jvm/default
export JAVA_OPTS='-Djruby.compile.invokedynamic=false'

if (( $enable_rvm )); then
  PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

  source ~/.rvm/scripts/rvm
  #rvm rvmrc trust ~/git/gless
  #rvm rvmrc trust ~/git/gless-bairyn
fi

source ~/.bashrc_ssh

alias pacman32="pacman --root /opt/arch32 --cachedir /opt/arch32/var/cache/pacman/pkg --config /opt/arch32/pacman.conf"
alias run32="schroot -c arch32 -p --"
alias schroot32="schroot -c arch32 -p --"

#export LANG=ru_RU.utf8
#export LANGUAGE=ru_RU.utf8

#alias mplayer-wl="mplayer -af scaletempo -fixed-vo -loop 0 -speed 1"

export LD_LIBRARY_PATH=~/lib:$LD_LIBRARY_PATH

export SDL_AUDIODRIVER=pulse

#alias snc="sudo netcfg"
alias snc="sudo netctl"
#export LANGUAGE=en LANG=en_GB.utf8

alias sos='while true; do sleep 2; for I in `seq 1 3`; do beep -f 400 -l 500; sleep 0.5; done; done'

alias updick='/usr/bin/uptime | perl -ne "/(\d+) d/;print 8,q(=)x\$1,\"D\n\""'

#wmname LG3D

#cowsay -f "$(ls /usr/share/cows/ | sort -R | head -1)" "$(fortune -s)"

alias lynx="lynx -accept_all_cookies"

alias beep-wl="beep -f 400 -l 100"

# require-tmux ensures you're in a tmux session.  If TMUX isn't set and isn't
# blank, and if TERM isn't/doesn't contain "screen", run "exec tmux $*" (by
# default, with no args, "tmux" is "tmux new").
function require-tmux()
{
  source ~/bin/require-tmux $*
}






# TODO: function to append paths separated by colons, including no colon if it
# doesn't exist!


declare -xg $(echo LD_LIBRARY_PATH)="${HOME}/local/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}"
declare -xg $(echo PATH           )="${HOME}/local/bin${PATH+:$PATH}"














":" << '":"'
# ... Oh!  *Why* was I writing this when mplayer can be configured? :D  See
# ~/.mplayer/config .
# Each pattern matches against all files in this directory.
#
# Usage:
#
#     mplayer-wl                                                           \
#       [-:     no initial_mlayer_args]                                    \
#       [+:     add default patterns]                                      \
#       [+pat:  add paths matching _pat_ before mplayer-args...]           \
#       [++grep_command: add paths filtered by command after file-find...] \
#       [--]
#       mplayer-args
#
# ORDER IS SIGNIFICANT!
function mplayer-wl {
  local -a mplayer_args

  local -A initial_mplayer_args
  initial_mplayer_args=(
    fix_pitch  '--af-add scaletempo'
    one_window '--fixed-vo'
    loop       '--loop 0'
  )

  if [[ "${1--}" != '-' ]] {
    shift
    mplayer_args+=("${(vz)initial_mplayer_args[@]}")
  }

  while [[ "${${1-x}[1]}" == '+' ]] {
    if [[ "$1" != '+' ]] {
      if [[ "${1[2]-x}" == '+' ]] {
        local pat="${1[2,-1]}"
        shift

        {eval "$(cat)"} < <(
          sed -r 's/^.*$/mplayer_args+=(&)/' < <(
            egrep -i "${1}" < <(
              find . -type f
            )
          )
        )
      } \
      else {
        local grep_command="${1[3,-1]}"
        shift

        {eval "$(cat)"} < <(
          sed -r 's/^.*$/mplayer_args+=(&)/' < <(
            {eval "${grep_command}"} < <(
              find . -type f
            )
          )
        )
      }
    } \
    else {
      shift

      {eval "$(cat)"} < <(
        sed -r 's/^.*$/mplayer_args+=(&)/' < <(
          egrep -i "\.(mp[[:digit:]]|wav|flac|mov|avi|ogg)$" < <(
            find . -type f
          )
        )
      )
    }
  }

  if [[ "${1-x}" == '--' ]] {shift}

  mplayer "${mplayer_args[@]}" "$@"
}
# ^^ ARGH WHY NO WORK -_-
#alias mplayer-wl="mplayer -af scaletempo -fixed-vo -loop 0 -speed 1"
#alias mplayer-wl="mplayer-wl-tmp"
#function mplayer-wl-tmp() {
  ##mplayer "$@" --af-add=scaletempo --fixed-vo --loop 0
  #mplayer "$@" --loop 0
#}

function mplayer-wl-tmp() {
  mplayer "$@" --loop=0
}
":"


# # mplayer.

alias mplayer=mplayerb--

alias 'mplayer-bin==mplayer2'

# ## General utilities.
function mplayer-profile {mplayer-bin --profile="$1" "${=argv[2,-1]}"}
aliasg   mplayer-p      = 'mplayer-profile'

# ## General aliases.
aliasg mplayer-wl       = 'mplayer-bin --profile=bairyn'

# ## Special profiles.
aliasg mplayer-default  = 'mplayer-bin --profile=default'
aliasg mplayer-defaults = 'mplayer-mplayer'
aliasg mplayer-profiles = 'mplayer-bin --profile=help'

# ## User profiles.
#
# TODO: dynamically search ~/.mplayer/profiles to create these aliases!
aliasg mplayer-mplayer  = 'mplayer-bin --profile=mplayer'
aliasg mplayer-bairyn   = 'mplayer-bin --profile=bairyn'

# ## Profile aliases.
aliasg mplayerb = 'mplayer-bairyn'
aliasg mplayerd = 'mplayer-defaults'  # *not* the default profile!
aliasg mplayerm = 'mplayer-default'   # (the default profile; mneumonically "main".)

# ### With media files.
aliasg mplayerb-- = 'with-media-files mplayer-bairyn'
aliasg mplayerd-- = 'with-media-files mplayer-defaults'  # *not* the default profile!
aliasg mplayerm-- = 'with-media-files mplayer-default'   # (the default profile; mneumonically "main".)

function media-files {
	while { (($#)) && shift }; do
		case "$(mimetype -b -- "$1")" in ((audio|video)/*) printf '%s\n' "$1" ;; esac
	done
}

function filter-media-files {
	xargs -0 -n1 zsh -c $'case "$(mimetype -b -L -- "$1")" in ((audio|video)/*) printf \'%s\\n\' "$1" ;; esac' '/!_filter-media-files'
}

function get-media-files {
	find . -xtype f -print0 | sort | filter-media-files
}

alias with-media-files='\with-media-files '
function with-media-files {
	"$@" "${(f)$(get-media-files)}" --loop=0
}

alias with-media-files-no-loop='\with-media-files-no-loop '
function with-media-files-no-loop {
	"$@" "${(f)$(get-media-files)}"
}


function disabled-ack {
  >&2 {
    echo $'bairyn\'s current ack preferences: note to self: please don\'t use ack.'
    return -1024
  }
}
function ack {
  disabled-ack "$@"
}
alias ack='disabled-ack'









# TODO: this is broken too -_-






declare -gi -x _BAIRYN_ZSH_GIT_SVN_NO_STD_SETTING

:;(){
local -i    setting_std=4096
local -i setting_no_std=8192
eval "function git-svn-enable-std  {_BAIRYN_ZSH_GIT_SVN_NO_STD_SETTING=${setting_std};}"
eval "function git-svn-disable-std {_BAIRYN_ZSH_GIT_SVN_NO_STD_SETTING=${setting_no_std};}"
eval "function git-svn-enabled-std {
  eval set '-o '{local_{options,patterns,traps},err_return}

  case \${==_BAIRYN_ZSH_GIT_SVN_NO_STD_SETTING} {
    (${setting_std})
      echo 1
      ;;
    (${setting_no_std})
      echo 0
      ;;
    (*)
      echo '-1'
      echo \"\$0: setting not yet set; call git-svn-(enable|disable)-std to configure.\" 1>&2
      return -1
  }
}"

function git-svn-std {
  eval set '-o '{local_{options,patterns,traps},err_return}

  local -i enabled="$(git-svn-enabled-std)"
  (($?==0)) || return $?

  if ((!$enabled)) {
    command git svn "$@"
  } \
  else {
    if (( $# < 1 )) {
      command git svn "$@"
    } \
    else {
      local -i no_std="${BAIRYN_ZSHRC_GIT_SVN_NO_STD:-0}"
      if (( !$no_std )) {
        local subcommand="$1"; shift
        command git svn "${subcommand}" --stdlayout --prefix='svn/' "$@"
      } \
      else {
        command git svn "$@"
      }
    }
  }
}
:;}

function git-svn-clone {
  eval set '-o '{local_{options,patterns,traps},err_return}
  git-svn-std clone "$@"
}

# All options are passed to init.
function git-svn-clone-shallow-1 {
  eval set '-o '{local_{options,patterns,traps},err_return}
  git-svn-clone -r HEAD "$@"

  #if [[ ! -o interactive ]] {
  #  1>&2 {
  #    echo "$0: must be run interactively."
  #    return -512
  #  }
  #}
  ## > It accepts all arguments that the init and fetch commands accept; with the
  ## > exception of --fetch-all and --parent.
  ##git-svn-std clone --parent "$@"
  #git-svn-init "$@"
  #1>&2 echo "$0: Complete the 'cd' command to cd into the directory; the function will then fetch with --parent, only fetching the SVN parent from the current HEAD"
  #pushln '(){"$@" && _part-2_git-svn-clone-shallow-10;} cd ./'
}
#function _part-2_git-svn-clone-shallow-10

function git-svn-init {
  eval set '-o '{local_{options,patterns,traps},err_return}

  git-svn-std init "$@"
}

function git-svn {
  eval set '-o '{local_{options,patterns,traps},err_return}

  if [[ "${1-x}" == 'clone' || "${1-x}" == 'init' ]] {
    git-svn-std "$@"
  } \
  else {
    command git svn "$@"
  }
}

alias git='check-git-svn'
function check-git-svn {
  eval set '-o '{local_{options,patterns,traps},err_return}

  if [[ "${1-x}" == 'svn' ]] {
    1>&2 {
      echo '(^)'"$0: git does not use --stdlayout and --prefix by default."
      echo '(.)'"$0:"
      echo '(.)'"$0: Use 'git-svn' instead, or use 'command git svn' if you…"
      echo '(.)'"$0: …really want to do this."
      echo '(.)'"$0:"
      echo '(.)'"$0: The 'git-svn' shell function will call 'git-svn-std' if…"
      echo '(.)'"$0: …the subcommand is either 'clone' or 'init'."
      echo '(.)'"$0:"
      echo '(.)'"$0: The 'git-svn-std' shell function will add…"
      echo '(.)'"$0: …\"--stdlayout --prefix='svn/'\" before the subcommand."
      echo '(.)'"$0:"
      echo '(.)'"$0: (This function, '${(q-)0}', should be defined as an…"
      echo '(.)'"$0: …alias for 'git'."
      echo '(.)'"$0:"
      echo '(.)'"$0: Sorry for that; wanted to save you from recovering from…"
      echo '(.)'"$0: …a git-svn clone without these options while maintaining…"
      echo '(.)'"$0: …interopability with SVN."
      echo '(.)'"$0:"
      echo '($)'"$0: Aborting."
      return -1024
    }
  } \
  else {
    command git "$@"
  }
}



# Allow nesting TMUX sessions without manually unsetting TMUX.
typeset -gxT TMUX_SESSIONS="${TMUX_SESSIONS-"${TMUX-}"}" tmux_sessions $'\x1F'
if [[ "${TMUX+y}${TMUX_+y}" == 'yy' && "${TMUX}" != "${TMUX_}" ]] { tmux_sessions+="${TMUX}" ;}
if (( $+TMUX )) { export TMUX_="${TMUX:-TMUX_}" && unset TMUX ;}


## Allow nesting TMUX sessions without manually unsetting TMUX.
#export TMUX_="$TMUX"
#unset TMUX

#echo 'TODO: BUY MAGNESIUM!'

#echo $'\e[31mTODO: read ~/TODO!\e[0m'
#echo 'TODO: https://github.com/seebi/dircolors-solarized!'

# TODO: append!
chpwd() {
  chpwd_ls "$@"
}

chpwd_ls() {
  emulate -LR zsh
  set -o no_clobber -o pipe_fail -o no_unset -o err_return

  # ARGH FIXME (filter stuff)
  return 0

  declare -gix CD_LS_ENABLED="${CD_LS_ENABLED-1}"
  declare -gx  CD_LS_CMD="${CD_LS_CMD-}"
  declare -gx  CD_LS_CMD__SUGGESTION="${CD_LS_CMD__SUGGESTION-cd}"
  declare -gx  CD_LS_TIMEOUT="${CD_LS_TIMEOUT-0.1s}"
  declare -gxT CD_LS_ARGS_POST="${CD_LS_ARGS_POST-"-p,-h,--hide-control-chars,--color=always"}" cd_ls_args_post ','

  declare -gx  CD_LS_MSG_MORE="${CD_LS_MSG_MORE-more…}"
  declare -gx  CD_LS_MSG_FIT="${CD_LS_MSG_FIT-}"
  declare -gxT CD_LS_MSG_CODES="${CD_LS_MSG_CODES-7}" cd_ls_msg_codes ':'
  declare -gx  CD_LS_MSG_INDENTATION="${CD_LS_MSG_INDENTATION-  }"

  declare -gix CD_LS_DISABLED_NEXT=${CD_LS_DISABLED_NEXT-$((false))}

  if (( ${CD_LS_DISABLED_NEXT-1} )) { CD_LS_DISABLED_NEXT=$((false)); return 0 }

  if (( !$CD_LS_ENABLED ))                                                              { return 0 }
  zmodload zsh/parameter
  if [[ -n "${CD_LS_CMD}" &&  $( (){ echo "$1" } "${(z)history[$HISTCMD]}" ) != 'cd' ]] { return 0 }

  # ~~~

  local line

  local indentation="${CD_LS_MSG_INDENTATION}"
  local format="$(printf '%s%s%%s\e[0m ' "$(printf '\e[%dm' "${=cd_ls_msg_codes[@]}")" "$indentation")"

  local many="$CD_LS_MSG_MORE" && local manyf="$(printf "$format" "$many")"
  local  fit="$CD_LS_MSG_FIT"  && local  fitf="$(printf "$format" "$fit" )"

  local -i ls_cols=$((  ${COLUMNS-80} - $#indentation - $#many - 1  ))

  local quote="$(echo -ne $(echo -ne                                          \
    $'function() {'                                                           \
    $'  local escape="(\e\\[[[:digit:];]+m)";'                                \
    $''                                                                       \
    $'  local prefix="$(env -i egrep -o "^${escape}*"                <<< "$1")";' \
    $'  local suffix="$(env -i egrep -o "(${escape}+/)?${escape}*\$" <<< "$1")";' \
    $'  local string=${(q-)${1[$(( $#prefix + 1 )),$(( -$#suffix - 1 ))]}};'  \
    $''                                                                       \
    $'  ls_cols+=$(( $#prefix + $#suffix ))'                                  \
    $''                                                                       \
    $'  printf \'%s%s%s\\n\''                                                 \
    $'    "$prefix"'                                                          \
    $'    "$string"'                                                          \
    $'    "$suffix"'                                                          \
    $'}'                                                                      \
  ))"

  local output_filter=$'sed -re\'s/^$//;t; s/^(.{\'"$ls_cols"\'}).+/\'"$manyf"\'\1/;t; s/^.*$/\'"$fitf"\'&/; q\''

  if ! (
    {timeout "${=CD_LS_TIMEOUT}" ls -1 "${=cd_ls_args_post[@]}" || return $?} >>(
       { local l1; while { read -ru0 l1 } { eval "$quote" "${(q-)l1}" } }     >>(
       { paste -s -d ' ' }                                                    >>(
       { sponge }                                                             >>(
       { local l2; while { read -ru0 l2 } { echo -ne "$l2" | eval "${output_filter}" } } >>(
    cat)))))
  ); then
    echo TIMED OUT
    printf "$format\n" "$(printf 'timed out (%q)…' ${==CD_LS_TIMEOUT})"
  fi

  return 0








  if ! (
    {timeout "${=CD_LS_TIMEOUT}" ls -1 "${=cd_ls_args_post[@]}" || return $?} >>( \
        { while { read -ru0 line } { eval "$quote" "${(q-)line}" } }          | \
       paste -s -d ' '                                                        | \
       sed -re's/^$//;t; s/^(.{'"$ls_cols"'}).+/'"$manyf"'\1/;t; s/^.*$/'"$fitf"'&/; q' \
    )
  ); then
    echo TIMED OUT
    printf "$format\n" "$(printf 'timed out (%q)…' ${==CD_LS_TIMEOUT})"
  fi

  return 0




  # OLD
  local -a cmdline && set -A $_ ls --color=always --hide-control-chars -h -C -w ${COLUMNS-} -p

  local -i input=-1 output=-1

  local    line
  local -a lines
  local    getLines=$'{ lines=() && while { read -u $output -r line ;} { lines+="${line}" ;}; printf \'\nend\n\' >&$input ;}'
  local    doneFifo=$'printf \'\n\n\' >&$input'

  {
    set -o no_monitor
    coproc {
      local line
      local next_line=$'read -t0.5 -ru0 line || { 1>&2 echo \'chpwd callback: tmp fifo for ls timed out.\'; return 2 }'
      while { eval $next_line }; do
        if [[ -z "$line" ]]; then
          eval "${next_line}"
          if [[ -z "${next_line}" ]] { exit 0; break }
        fi

        printf '%s\n' "$line"
      done
    }

    exec {input}>&p {output}<&p

    local format='\e[%dm'
    local message='%2$s\e[0m%1$s'
    if   >&$input { ! timeout 0.1 "${cmdline[@]}" ;}; then
      eval $getLines

      printf "$format" 1
      printf "$message"  ' '  'timed out…'

      eval $doneFifo
    else
      eval $getLines

      case $#lines in
        (0)
          ;;
        (1)
          printf '%s\n' "${lines[1]}"
          ;;
        (*)
          printf "$format" 1
          printf "$message"  ' '  'more…'
          ;;
      esac

      eval $doneFifo
    fi
  } always { if (( $input >= 0 )) { exec {input}>&- }; if (( $output >= 0 )) { exec {output}>&- } ;}

  wait
}
declare -g CD_LS_DISABLED_NEXT=$((true))
chpwd_ls

##alias info="info --vi-keys"
##alias info='2>&1() printf "$(Disabled (alias))". #'
#
#
#
#
#function info-where {
#  \info -w "$@"
#} && alias info{w,{w,-w:}'(info-where)'}=info-where
#
#function info-man {
#  man <(info2man $(info-where "$@"))
#} && alias info{,-}m{,'(info-man)'}=info-man
#
#function info-editor {
#  "$(sensible-editor)" =(\info "$@")
#} && alias info{,-}e{,'(info-editor)'}=info-editor
#
#: pager && function "info-$_" {
#  "${$(whence sensible-pager||:):-less}" =(\info "$@")
#} && alias info{,-}"${_[1]}"{,'(info-pager)'}=info-pager
#
#
#
#
##function info-where {
##  \info -w "$@"
##} && alias info{w,{w,-w:}'(info-where)'}=info-where
##
##function info-man {
##  man <(info2man $(info-where "$@"))
##} && alias info{,-}m{,'(info-man)'}=info-man
##
##function info-editor {
##  "$(sensible-editor)" =(\info "$@")
##} && alias info{,-}e{,'(info-editor)'}=info-editor
##
##: pager && function "info-$_" {
##  "${$(whence sensible-pager||:):-less}" =(\info "$@")
##} && alias info{,-}"${_[1]}"{,'(info-pager)'}=info-pager
#
#
#function od-hex {
#  od -A x -t x2z -v "$@"
#} && alias odh{,'(od-hex)'}=od-hex
#
#function od-hex {
#  od -A x -t x2z -v "$@"
#} && alias odh{,'(od-hex)'}=od-hex
#
## This must be at the end of .zshrc, because of an optional "exec" statement.
#source ~/.priv/zshrc

















declare -g CD_LS_DISABLED_NEXT=$((true))
chpwd_ls

#alias info="info --vi-keys"
#alias info='2>&1() printf '%s\n' "$(printf 'Disabled (alias)')" #'

alias info=infom

function info-where {
  \info -w "$@"
} && alias infow=info-where

# TODO: search for each man page with MANOPT?, or at least filter out incompatible man page.
function info-html {
  env -u MANOPT BROWSER="${BROWSER:-links2 -g}" \
    man -l --html $(read -t0 -eru0||:) -- - <<(info2man $(info-where "$@"))
} && alias info{-,}h{,'(info-html: man --html)'}=info-html
function info-man {
  man -l $(read -t0 -eru0||:) -- - <<(info2man $(info-where "$@"))
} && alias info{-,}m{,'(info-man)'}=info-man
function info-editor {
  "$(whence sensible-editor||whence pager||whence vim||whence vi)" $(read -t0 -eru0||:) =(\info "$@")
} && alias info{-,}e{,'(info-editor)'}=info-editor
function info-pager {
  "$(whence sensible-pager||whence pager||whence less)" $(read -t0 -eru0||:) =(\info "$@")
} && alias info{-,}p{,'(info-pager)'}=info-pager

function info-vim {
  vim -R <(\info "$@")
} && alias infov{,'(info-vim)'}=info-vim
function info-less {
  less -f <(\info "$@")
} && alias infol{,'(info-less)'}=info-less

# TODO (see inside)
#function doc {
#  set -o local_options -o local_patterns -o local_traps
#  set -o no_monitor
#
#  # TODO: fix your MANOPT aliases!
#  local MANOPT; unset MANOPT
#
#  local suffix='m'
#  local command='infom'
#  local -i endopts=$((false))
#  local opt_setting_to
#
#  local -a args
#  set -A args "$@"
#
#  while (( $#args >= 1 )) { {
#    local arg="$args[1]"; shift args
#
#    # Options.
#    if [[ "${arg[1,3]:-x}" == '--!' ]] {
#      args=("${(s. .)argst}" "$args[@]")
#    }
#
#    if [[ -z "${opt_setting_to}" ]] {
#      set "$opt_setting_to" "$arg"
#      opt_setting_to=$((false))
#      continue;
#    }
#
#    if [[ !$(($endopts)) && "${v[1]-x" == '-' ]] {
#      case "${v[2,-1]:-}"
#        (h|-help)
#          1>&2 {
#            printf 'Usage: %s [-H | -? | -he[lp]] [-V | -ve[rsion]] [-c CMD | -c[ommand]=CMD]{CMD-info} [-s CMDSUFFIX] [--] [MAN_PAGE|MAN_PAGE_REGEX|TEX_INFO `(info docs)`]…\n' "$0"
#            return -1
#            ;;
#          }
#        (V|-version)
#          # TODO
#          printf '0.1.TODO\n' "$0"
#              echo TODO >&2; return -4# TODO
#          return 0
#          ;;
#        (-command=*)
#        (?*)
#          1>&2 {
#            printf 'Unrecognized option: ; \n'
#            exit -3
#          }
#          ;;
#      esac
#    }
#
#    # Start a coproc in the background to find man pages; it may take some
#    # time.
#
#    coproc {
#    }
#
#    #infom "$v"
##^.[$()|*+?{\
#    # 
#    #
#  } always { coproc exit && wait } }
#}




#bar=$(
#)
#foo="eval eval \"\$(base64 -d <<< $bar)\""


#{ set MACRO "$(printf '' "$(cat)")}" } <<($(<<;

#alias -g '!!:_MACRO'
#declare -gx MACRO="${MACRO:-$(<<$'\n'
#  hello there       test
#)}"
#debug-args "${MACRO}"



#printf '\e]710;%s\007' "9x15bold,xft:Kochi Gothic"

alias help=run-help

function font-set-suggestions-old {
  local -i interactive=-1 names=-1
  {
    if ((!$#))
      then exec {interactive}>&1 {names}>>/dev/null
      else exec {names}>&1       {interactive}>>/dev/null
    fi

    >&$interactive printf '%s\n\n' '% font-set 1'
    echo "$( \
      >>(>&$interactive { sed -re's|^ *||' -e$'s|(:)([^:]*)|\\1\e[1m\\2\e[0m|' -e= | xargs -n2 -d'\n' printf '(%2d) %s\n' ;}) \
      >>(>&$names { sed -re's|^ *||' }) \
      head -n -1<<$'\n'
        xft:Monaco:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
        xft:Droid Sans Mono:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
        xft:monofur:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
        xft:Terminus:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
        xft:Utopia:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
        xft:Code2000:antialias=false
        xft:Monaco:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Droid Sans Mono:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:monofur:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Terminus:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Utopia:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:DejaVu Sans Mono:style=Book:antialias=false:size=10
        xft:FreeMono:style=Regular:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Code2000:antialias=false
        x:-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        x:-gnu-unifont csur-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        x:-gnu-unifont sample-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        xft:Unifont:style=Medium
        xft:Unifont Upper:style=Medium
        xft:Unifont CSUR:style=Medium
        xft:Unifont Upper CSUR:style=Medium
    )"
  } always {
    if (($interactive >= 0)) exec {interactive}>&-
    if (($names       >= 0)) exec {names}>&-
  }
}

function font-set-suggestions {
  #local interactive=1 names=1
  {
    # … *Why* does this break without those printfs on an argument? Huh!
    if ((!$#))
      then { printf ''; exec 8>/dev/null 9>&1 ;}
      else { printf ''; exec 9>/dev/null 8>&1 ;}
    fi

    >&9 printf '%s\n\n' '% font-set 1'
    echo "$( \
      >>(>&9 { sed -re's|^ *||' -e$'s|(:)([^:]*)|\\1\e[1m\\2\e[0m|' -e= | xargs -n2 -d'\n' printf '(%2d) %s\n' ;}) \
      >>(>&8 { sed -re's|^ *||' }) \
      head -n -1<<$'\n'
        xft:Noto Serif:style=Regular:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=9
        xft:Droid Serif:style=Regular:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:DejaVu Serif:style=Book:antialias=false:size=10
        xft:monofur:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:FreeSerif:style=Regular:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Utopia:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
        xft:Code2000:antialias=false
        x:-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        x:-gnu-unifont csur-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        x:-gnu-unifont sample-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
        xft:Unifont:style=Medium
        xft:Unifont Upper CSUR:style=Medium
    )"
#      head -n -1<<$'\n'
#        xft:Monaco:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
#        xft:Droid Sans Mono:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
#        xft:monofur:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
#        xft:Terminus:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
#        xft:Utopia:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10
#        xft:Code2000:antialias=false
#        xft:Monaco:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:Droid Sans Mono:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:monofur:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:Terminus:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:Utopia:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:DejaVu Sans Mono:style=Book:antialias=false:size=10
#        xft:FreeMono:style=Regular:antialias=true:hinting=true:hintstyle=hintfull:pixelsize=10
#        xft:Code2000:antialias=false
#        x:-gnu-unifont-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
#        x:-gnu-unifont csur-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
#        x:-gnu-unifont sample-medium-r-normal-sans-16-160-75-75-c-80-iso10646-1
#        xft:Unifont:style=Medium
#        xft:Unifont Upper:style=Medium
#        xft:Unifont CSUR:style=Medium
#        xft:Unifont Upper CSUR:style=Medium
#    )"
  } always {
    #if (($interactive >= 0)) exec {interactive}>&-
    #if (($names       >= 0)) exec {names}>&-
  }
}

# Pass format string to be re-formatted depending on whether $TMUX or $TMUX_ is
# set, or no arguments to unconditionally print a format string that embeds a
# format string for tmux.
function escape-code-bypass-tmux {
  if (( $# <= 0 )); then
    printf '%s\n' '\ePtmux;\e%s\e\\'
    return
  fi

  if ! [[ ! -n "${TMUX-}${TMUX_-}" ]]
    then  # not tmux
      printf '%s' "$@"
    else  # tmux
      # Thanks <http://blog.yjl.im/2014/12/passing-escape-codes-for-changing-font.html>!
      local cmd="printf $(escape-code-bypass-tmux)"
      $=cmd "$1" && while (( shift )) { $=cmd "$1" ;}
  fi
  echo
}

function font-set-format-string {
  if (( $# <= 0 ))
    then
      if [[ -z "${TMUX-}${TMUX_-}" ]] then printf '%s\n' '\ePtmux;\e\e]710;%s\007\e\\'
                                      else printf '%s\n' '\ePtmux;\e\e]710;%s\007\e\\'
      fi
    else printf "$(font-set-format-string)" "$@"
  fi
}

function font-reset {
  printf "$(font-set-format-string)" 'xft:Monaco:antialias=true:hinting=true:hintyle=hintfull:pixelsize=10'
}

# If any non-first arguments are non-integral, return the left-most position,
# starting at index 1, which can be overridden by passing a representation
# other than that of index 1 as the first argument, but at a lower offset,
# greater than `-$#`, 0 loses its signal of success to ambiguity; and a
# maximum of one less (`-$# - 1`) is required to disambiguate this shell
# function's signal for failure in the first argument, -1.
#
# Currently, such `{-1..0}` intersecting `{$[ -$# - 1 ]..-1}` `$1` values are
# silently processed generally despite the ambiguity.  This is liable to
# change, with an option to configure original relevant behaviour.
#
# Usage:
#   is-numeric -- arg1 arg2 arg3 arg4
#   is-numeric -1 arg1 arg2 arg3 arg4
#
#   is-numeric -8 arg1 arg2 arg3 arg4  # Start at offset 8, representing
#   `arg1`.  If for example arg2 is non-integral
#                                      # represent arg1 and be returned if .
#
# (When the first argument does not begin with a dash, it may be .)
function is-numeric {
set -x
  local -i pos=1
  if [[ "${1[1]-x}" == '-' ]] {
    if ! [[ "$1" == '--' || "$1" == '-1' ]] {
      : '[31m:::::::::::::::::::::::::::::::::[39m' :
      : '[39m~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[39m' :
      is-numeric -- "${1[2,-1]}" || return $[-$?]
      pos="${1[2,-1]}"
    }

    shift
  }

  if (( !$# )) return 0;

  if { ! egrep -q -m1 '^(-?[[:digit:]]+|--)$' <<< "${1-x}" ;}  return $pos

  shift
  is-numeric "-$[pos+1]" "$@"
}

# Thanks printf '\ePtmux;\e\e]710;%s\007\e\\' "$FONT" !
function font-set {
  if   (( $# <= 0 )); then
    font-set-suggestions
  elif (( $# == 1 )) && egrep -q '^[[:digit:]]+$' <<< "$1"; then
    printf "$(font-set-format-string)" "$(<<(font-set-suggestions --names) sed -nre"$1{p;q}")"
  else
    printf "$(font-set-format-string)" "$@"
  fi
}
#printf '\e]710;%s\007' "9x15bold,xft:Kochi Gothic"


export MANOPT='-a'
alias manr='man --regex'




function pulseaudio-list-manpages {
  apropos --regex 'pulse' \
  |
  # Highlighting.
  egrep --color=always -i \
    -e '(^pa?)?' -e '(\<pa\>)?' -e '([Pp]ulse([Aa]udio)?)?' \
  |
  sort | uniq
}


## Thanks <http://serverfault.com/a/563316/278114>!
#function xpipe-subshell {
#  local -a arg_required arg_optional arg_none
#  set -A arg_required
#  set -A arg_optional TODO
#  set -A arg_none     '-?' --help -V --version
#
#  local -A options_args
#  local -a options_noargs
#  local -a nonoptions
#  local -a unspecified_options
#
#  local -i i=1
#  while (( $i <= $# )) {
#    local arg=
#  }
#  while (( $# >= 1 )) && 
#
#  local -a overrides
#  local -a opts && set -A "$_" '-I' '__xpipe-subshell_%__'
#  while { (( $# >= 1) && [[ "$1[1]" == '-' && "$1" != '--' ]] ;}; do {
#    if [[ "$1"          == '--' ]] break
#    if [[ "${1[1,2]-x}" == '-n' ]]
#    opts+="$1"
#  } always { shift ;} done
#
#  xargs -I '__xpipe-subshell_%__' "$opts[@]" -- "sh -c$@"
#}
#
#function each-line {
#  <<("${@:-cat}") \
#    while { read -eru0 ;} {}
#} && function xpipe-alt { each-line "$@" ;}
#
#function each-line-filter {
#}
#
#function each-line-append {
#}
#
#
## Adds delay between before each line of input.
##
## Can be used in a pipe, e.g. to not display all lines of input at once.
#function slow {
#  local argi
#  for argi ($(seq $argc)) {
#  }
#}

function google-url {
	local url='https://google.com'
	if (( $# > 0 )); then
		local encoded="$*"
		encoded="$(<<< "${encoded}" perl -MURI::Escape -ne 'chomp;print uri_escape($_),"\n"')"

		url="$(printf '%s%s%s' "$url" '/search?q=' "$encoded")"
	fi

	printf '%s\n' "$url"
}

function google-with {
	local with="${1-default}"
	#local url="eval eval$(printf ' %q' 'url="$(google-url "$*")"')"
	#local run="eval eval$(printf ' %q' '$(printf "$==cmd" "$==url")')"
	local cmd=$'sensible-browser %s'

	if (( $# >= 1 )) shift

	local procedure="$(printf 'eval %q' \
			"$(printf ' %s' \
				'$(printf' \
					'"$==cmd"' \
					'"$==url"' \
				')' \
			)" \
		)"

	if 2>>/dev/null whence "${with}"; then
		printf '%s\n' 'Which command?  1 to abort.'
		select cmd in \
				'abort' \
				'%1$.0sreturn' \
				"%1$.0$(printf '%s %%1$s' "${with}")" \
				"%1$.0s$0$(printf ' %s' "$@")" \
				'%1$.0sreturn' \
			do break done

		if [[ "${cmd}" == 'abort' ]]; then
			return 4
		fi
	else
		case "${with}" in
			# User-supplied, and --help & --version.
			(with)
				if (($# < 1)); then
					cmd="$(printf '%s%s: %s' '%.0s' "$0" '%.0s Sorry, no with-specifier was specified.')"
				else
					cmd="$(printf '%s%s' "$1" ' %s')"
					shift
				fi
				;|

			(--help)
				printf '%s: %s\n' "$0" 'TODO-USAGE'
				return -2
				;;
			(--version)
				printf '%s\n' 'TODO-VERSION'
				return -3
				;;

			(*%*) ;&
			(* *)
				cmd="${with}"
				;;

			(cmd) ;&
			(command)
				if (($# < 1)); then
					cmd="$(printf '%s%s: %s' '%.0s' "$0" '%.0s Sorry, no command was specified.')"
				else
					cmd="$(printf '%s%s' "$1" ' %q')"
					shift
				fi

			(printf) ;&
			(fmt)    ;&
			(format)
				if (($# < 1)); then
					cmd="$(printf '%s%s: %s' '%.0s' "$0" '%.0s Sorry, no format was specified.')"
				else
					cmd="$(printf '%s' "$1")"
					shift
				fi
				;;

			# Modes: terminal, x11; can't end in single letter of mode.
			# terminal
			(*t?*)
				local DISPLAY
				unset DISPLAY
				if { 2>>/dev/null whence 'www-browser' ;} cmd='www-browser %q'
				;|
			# x11
			(*x?*)
				if { 2>>/dev/null whence 'x-www-browser' ;} cmd='x-www-browser %q'
				;|

			# Commands.
			# links2-g
			(*l*)
				cmd='links2 -g %q'
				;;
			# chromium
			(*c*)
				cmd='chromium %q'
				;;
			# sens, sensib  ("sensible-browser" would collide with "links2-g")
			(*s*)
				cmd='sensible-browser %q'
				;;

			# default
			(*d*)
				;&
			(*)
				if [[ -z "${cmd}" ]]; then
					cmd='sensible-browser %q'
				fi
				;;
		esac
	fi

	local url="$(google-url "$@")"
	eval "$(printf "${cmd}" "${url}")"

	##$(printf "$cmd" "${url}")
	#$=url
	#$=run
}

() { setopt local_{options,patterns,traps} err_return; setopt no_unset pipe_fail no_clobber; setopt multi_func_def
	function google {
		googlel "$@"
	}
	alias g='google'


	function googlet{,-terminal} {
		google-with terminal "$@"
	}
	function googlex{,-terminal} {
		google-with x11 "$@"
	}


	function google{l,-links2-g} {
		google-with links2-g "$@"
	}
	function google{c,-chromium} {
		google-with chromium "$@"
	}
	function google{s,-sensible-browser} {
		google-with sensible-browser "$@"
	}
}

alias exec='\exec '



zmodload zsh/mathfunc



alias lt='lt +A'


declare -gix BZSH_OD_GROUP_NUM_BYTES="${BZSH_OD_GROUP_NUM_BYTES:-1}"
function od-hex {
  local -i n="${BZSH_OD_GROUP_NUM_BYTES:-2}"
    od -A x -t x${n}z --endian=big "$@" \
  | sed --posix -nre "$(<< $'\n'
      p
    )"
} && alias odh{,'(od-hex)'}=od-hex

function od-hex-raw {
  local -i n="${BZSH_OD_GROUP_NUM_BYTES:-2}"
  od -v -A n -t x$n "$@" \
} && alias odh{,'(od-hex)'}=od-hex

#function od-hex-raw-dump {
#  local -i n="${BZSH_OD_GROUP_NUM_BYTES:-2}"
#    od -v -A n -t x1 -w1 "$@" \
#  | sed -re '=;s/^ //;'       \
#  | sed -nre '1~2s/^//'
#} && alias odh{,'(od-hex)'}=od-hex

function od-defaults {
  od -A o -t oS -w16 "$@"
} && alias odh{,'(od-hex)'}=od-hex



function rehash-locals {
	local dir
	local full

	local comp

	if (( !$+ld_library_path )) {
		typeset -gxT LD_LIBRARY_PATH ld_library_path
	}

	<<(find "$HOME/locals" -maxdepth 1 -mindepth 1 -xtype d '!' -name '.*' -print0 ) \
	while read -d '' -ru0 dir; do
		full="$(realpath -- "${dir}")"

		comp="${full}/bin"
		if (( !${path[(I)$comp]}            )) path+="${comp}"

		comp="${full}/lib"
		if (( !${ld_library_path[(I)$comp]} )) ld_library_path+="${comp}"
	done
}
rehash-locals

function rehash {
	if [[ "$*" != '-f' ]] { echo "currently only supported: $0 -f" >&2; return -1 ;}
	rehash-locals "$@" || { builtin rehash "$@" && rehash-locals "$@" }
	builtin rehash "$@"
}






## Thanks <http://stackoverflow.com/a/3613525/2899502>!
## This doesn't seem to produce reliable results for me; in particular
#function is-backgrounded-job
#case $(ps -o stat= -p $$) in
#	(*+*) return 1
#	(*)   return 0
#esac
#
#function eval_background-suspend {
#	if (( $# == 0 )) || 
#}
#
#
#
#
#function mkcd {
#}
#
#

#function display-notifications {
#	
#}
#
#periodic() {
#	display-notifications "$@"
#}




function return-status {
	{

		{
			1>> /dev/null 2>> /dev/null "$@"
		} && {
			printf '%d\n' "$?" || :
		}

	} || {

		{
			printf '%d\n' "$?"
		}

	}
}

function return-status-err {
	{

		{
			1>> /dev/null "$@"
		} && {
			printf '%d\n' "$?" || :
		}

	} || {

		{
			printf '%d\n' "$?"
		}

	}
}

function return-status-out {
	{

		{
			2>> /dev/null "$@"
		} && {
			printf '%d\n' "$?" || :
		}

	} || {

		{
			printf '%d\n' "$?"
		}

	}
}

# Don't suppress standard output and error.
function return-status-outerr {
	{ "$@" && { printf '%d\n' "$?" || : ;} ;} || printf '%d\n' "$?"
}

# Only print the return status when non-null.
function return-status-chronic {
	{

		{
			1>> /dev/null "$@"
		} && {
			printf '%d\n' "$?" || :
		}

	} || {

		{
			printf '%d\n' "$?"
		}

	}
}

## Interprets
#function repeat-arg {
#	if (($@[(I)--help])) {
#		printf 'Usage: TODO\n'
#		return -2
#	}
#	if (($@[(I)--version])) {
#		printf 'TODO!'
#		return -3
#	}
#
#	local -a processed_args
#	while (($#)) {
#		local arg="
#		if is-integer 
#	}
#	for arg ("$@") {
#		local -a pre_args
#	}
#
#
#	if (($# < 2)) {
#		() {
#			{
#				local degree_of_asshole=4
#				printf 'Sorry, currently only 2 arguments are supported.\n'
#
#				return 2
#
#			#	# ... no thanks!
#			#	printf 'UNDEFINED BEHAVIOUR encountered!\n'
#			#	printf 'Signal OHNOCODEFACETIOUSCODEFACETIOUSFORMATTHEHARDDRIVE caught.\n'
#			#	printf 'Formatting hard drive /dev/sdw ☠ …\n'
#			#	printf 'Forking into background …\n'
#			#	printf $'Interrupt by typing `(this isn\'t for real)` …\\n'
#			#	read -ru0 -d '' -k 12 -t "$degree_of_asshole"
#			#} always {
#			#	printf $'Just kidding!  I hope it was obvious enough!\n'
#			#	return 2
#			}
#		}
#	}
#	if (($#)) shift
#
#	local -a repeated_args
#	while [ "${1-x}" = '-1' ]; do
#		shift
#
#		pre_args += "$1"
#		shift
#	done
#
#	if ! is-integer "${1-x}"; then
#		1>&2 {
#		}
#	fi
#
#	local -i repetitions && let "$_ = ${1-0}"
#
#	local -a repeated
#	if (($repetitions >= 0)) {
#		repeat $repetitions repeated+="$1"
#		shift
#	} else {
#		repeat-arg ""
#		1>&2 {
#			printf 'A negative number of repetitions?  How do you mean?\n'
#			return 4
#		}
#		shift
#		repeat $repetitions return 4
#	}
#	repeat 
#	if (($# < 2 )) || [[ "$1" == '--help' || "$1" == '--version' ]]; then
#		printf 'Usage: TODO\n'
#		return '-2'
#	fi
#	if (( $#)) shift
#	if ((!$#)) {
#		1>&2 {
#			return -2
#		}
#	}
#	(($#))  || 
#	repeat $repetitions "$@"
#}

function repeat-pipe {
	# TODO
	#if (($@[(I)--help])) {
	#	printf 'Usage: TODO\n'
	#	return -2
	#}
	#if (($@[(I)--version])) {
	#	printf 'TODO!'
	#	return -3
	#}
	if (($#<2)) {
		1>&2 {
			printf 'TODO: need more args'
			return -4
		}
	}
	local -i count="$1"
	shift
	if (($count < 0)) {
		1>&2 {
			printf 'TODO: non-negative count'
			return -4
		}
	}

	if (($count==0)) {
		return 0
	} else {
		let count-=1
		"$@" | repeat-pipe "$count" "$@"
	}
}

function shift-up {
	sed -nre'1h;1!p;${g;p}'
}

function shift-down {
	sed -nre'$!{1h;1!H};${1!G;p}'
}

function return-status-hub {
	local cmd="return-status${${1+"-$1$(shift)"}:-}"
	"$cmd" "$@"
}




# This must be at the end of .zshrc, because of an optional "exec" statement.
source ~/.priv/zshrc
