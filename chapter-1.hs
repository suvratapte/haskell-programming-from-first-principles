-- Chapter 1 exercises

-- Equivalence exercises

-- λxy.xy = λmn.mz           -- b

-- λxy.xxy = λa.(λb.aab)     -- c

-- λxyz.zx = λtos.st         -- b


-- 1.11 Chapter Exercises

-- Combinators
-- λx.xxx - Yes
-- λxy.zx - No
-- λxyz.xy(zx) - Yes
-- λxyz.xy(zxy) - Yes
-- λxy.xy(zxy) - No

-- Normal form or diverge?
-- λx.xxx - Normal
-- (λz.zz)(λy.yy) - Diverge
-- (λx.xxx)z - Normal

-- Beta reduce

-- 1
-- (λabc.cba)zz(λwv.w)
-- (λa.(λbc.(cba)))zz(λwv.w)
-- (λa.(λb.(λc.(cba))))zz(λwv.w)
-- (λb.(λc.(cbz)))z(λwv.w)
-- (λc.(czz))(λwv.w)
-- (λwv.w)zz
-- (λw.(λv.w))zz
-- (λv.z)z
-- z

-- 2
-- (λx.(λy.xyy))(λa.a)b
-- (λy.(λa.a)yy)b
-- (λy.yy)b
-- bb

-- 3
-- (λy.y)(λx.xx)(λz.zq)
-- (λx.xx)(λz.zq)
-- (λz.zq)(λz.zq)
-- (λz.zq)q
-- qq

-- 4
-- (λz.z)(λz.zz)(λz.zy)
-- (λz.zz)(λz.zy)
-- (λz.zy)(λz.zy)
-- (λz.zy)y
-- yy

-- 5n
-- (λx.λy.xyy)(λy.y)y
-- (λx.λy.xyy)(λa.a)y
-- (λy.(λa.a)yy)y
-- (λy.yy)y
-- yy

-- 6
-- (λa.aa)(λb.ba)c
-- (λb.ba)(λb.ba)c
-- (λb.ba)ac
-- aac

-- 7
-- (λxyz.xz(yz))(λx.z)(λx.a)
-- (λx.λy.λz.xz(yz))(λm.n)(λp.q)
-- (λy.λz.(λm.n)z(yz))(λp.q)
-- (λz.(λm.n)z(λp.q)z)
-- (λz.nq)
