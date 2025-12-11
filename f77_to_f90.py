#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
F77 转 F90 格式转换脚本

处理流程：
1. 符号标准化：行号、TAB、缩进、空格、关键字
2. 语法转换：注释、类型、续行、IF、FORMAT等
3. 后处理：END后内容、文件头
"""

import re, sys


# ============ 工具函数 ============

def is_comment(line):
    """判断是否为注释行"""
    return line.lstrip().startswith('!')


def ensure_newline(line):
    """确保行尾有换行符"""
    return line if line.endswith('\n') else line + '\n'


# ============ 第一阶段：符号标准化 ============

def normalize_linenum(line):
    """移除行号前缀"""
    return re.sub(r'^\s*\d+→', '', line)


def normalize_tab(line):
    """将TAB展开为空格，第1-6列TAB展开到第6列"""
    if '\t' not in line:
        return line
    result, col = [], 0
    for c in line:
        if c != '\t':
            result.append(c)
            col += 1
        else:
            spaces = max(1, 6 - col)
            result.append(' ' * spaces)
            col = max(6, col + 1)
    return ''.join(result)


def normalize_spaces(line):
    """标准化空格：DO语句、指数格式、GOTO"""
    if is_comment(line):
        return line
    line = re.sub(r'\bDO\s*(\d+)([A-Z])', r'DO \1 \2', line, flags=re.I)
    line = re.sub(r'(\d)[DE]\s+(\d)', r'\1D\2', line, flags=re.I)
    line = re.sub(r'\bGOTO\b', 'GO TO', line, flags=re.I)
    return line


def normalize_indent(line):
    """标准化缩进和标签格式"""
    if not line.strip() or is_comment(line):
        return line
    stripped = line.lstrip()
    # 标签行格式化
    m = re.match(r'^(\d+)(\s+)(\S.*)$', stripped)
    if m:
        return ensure_newline(f'   {m.group(1)} {m.group(3).rstrip()}')
    # 过深缩进标准化
    if len(line) - len(stripped) > 10:
        return ensure_newline('      ' + stripped.rstrip())
    return line


def normalize_keywords(line):
    """关键字转为大写"""
    if is_comment(line):
        return line
    kws = 'function|subroutine|implicit|do|end|if|then|else|endif|return|go to|call|continue|format|write|read|data|dimension|common|parameter|entry'
    return re.sub(r'\b(' + kws + r')\b', lambda m: m.group().upper(), line, flags=re.I)


# ============ 第二阶段：语法转换 ============

def conv_comment(line):
    """第1列C/c/*/!转为注释"""
    return '!' + line[1:] if line and line[0] in 'Cc*!' else line


def conv_type(line):
    """转换数据类型 REAL*8/COMPLEX*16"""
    line = re.sub(r'\bREAL\*8\b', 'REAL(8)', line, flags=re.I)
    line = re.sub(r'\bCOMPLEX\*16\b', 'COMPLEX(8)', line, flags=re.I)
    return re.sub(r'(IMPLICIT\s+)REAL\*8', r'\1REAL(8)', line, flags=re.I)


def conv_hollerith(line):
    """转换Hollerith格式nHxxx为字符串"""
    result, i = [], 0
    while i < len(line):
        m = re.match(r'(\d+)H', line[i:])
        if m and i + len(m.group(0)) + int(m.group(1)) <= len(line):
            n = int(m.group(1))
            start = i + len(m.group(0))
            text = line[start:start+n].replace("'", "''")
            result.append(f"'{text}'")
            i = start + n
        else:
            result.append(line[i])
            i += 1
    return ''.join(result)


def conv_format(line):
    """修复FORMAT语句开头逗号"""
    return re.sub(r'(FORMAT\s*\()\s*,', r'\1', line, flags=re.I)


def conv_pause(line):
    """PAUSE转为PRINT"""
    return re.sub(r'\bPAUSE\b', 'PRINT *,', line, flags=re.I)


def conv_end(line):
    """标准化END语句"""
    return '      END\n' if re.match(r'^\s*end\s*$', line, re.I) else line


def conv_arith_if(lines):
    """算术IF转换为IF-GOTO结构"""
    pattern = re.compile(r'^(\s*)(\d*\s*)IF\s*\((.+)\)\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*$', re.I)
    result = []
    for line in lines:
        m = pattern.match(line)
        if not m:
            result.append(line)
            continue
        ind, lab, expr, l1, l2, l3 = m.groups()
        base, pad = ind + lab, ind + '      '
        result.extend(_gen_arith_if(base, pad, expr, l1, l2, l3))
    return result


def _gen_arith_if(base, pad, expr, l1, l2, l3):
    """生成算术IF的等价IF-GOTO代码"""
    if l1 == l2 == l3:
        return [f"{base}GO TO {l1}\n"]
    if l1 == l2:
        return [f"{base}IF (({expr}) .GT. 0) GO TO {l3}\n", f"{pad}GO TO {l1}\n"]
    if l2 == l3:
        return [f"{base}IF (({expr}) .LT. 0) GO TO {l1}\n", f"{pad}GO TO {l2}\n"]
    if l1 == l3:
        return [f"{base}IF (({expr}) .EQ. 0) GO TO {l2}\n", f"{pad}GO TO {l1}\n"]
    return [f"{base}IF (({expr}) .LT. 0) GO TO {l1}\n",
            f"{pad}IF (({expr}) .EQ. 0) GO TO {l2}\n", f"{pad}GO TO {l3}\n"]


def conv_continuation(lines):
    """处理续行：第1列数字或第6列非空字符"""
    result = []
    for line in lines:
        cont_line = _get_continuation(line, result)
        if cont_line:
            result[-1] = result[-1].rstrip('\n') + ' &\n'
            result.append(cont_line)
        else:
            result.append(line)
    return result


def _get_continuation(line, result):
    """检查并返回续行内容，非续行返回None"""
    if is_comment(line) or not result or not result[-1].strip():
        return None
    # 第1列数字表示续行
    if line and line[0].isdigit():
        content = line[1:].lstrip()
        return '      ' + content if content.strip() else None
    # 第6列非空表示续行（排除标签行）
    if len(line) > 5 and line[5] not in (' ', '0', '\n'):
        label = line[:5].strip()
        if not (label and label.isdigit()):
            return '      ' + line[6:]
    return None


def fix_do_loops(lines):
    """修复DO循环：共享标签分配新标签，非CONTINUE终止转为CONTINUE"""
    ctx = {'result': [], 'new_label': 9000, 'do_stack': [], 'pending': {}}
    for line in lines:
        if not _handle_do_stmt(line, ctx) and not _handle_label_stmt(line, ctx):
            ctx['result'].append(line)
    return ctx['result']


def _handle_do_stmt(line, ctx):
    """处理DO语句，共享标签分配新标签"""
    m = re.match(r'^(\s*)DO\s+(\d+)(\s+.+)$', line, re.I)
    if not m:
        return False
    indent, label, rest = m.groups()
    if label in ctx['do_stack']:
        ctx['new_label'] += 1
        new_lab = str(ctx['new_label'])
        ctx['result'].append(f"{indent}DO {new_lab}{rest}\n")
        ctx['pending'][new_lab] = label
        ctx['do_stack'].append(new_lab)
    else:
        ctx['do_stack'].append(label)
        ctx['result'].append(line)
    return True


def _handle_label_stmt(line, ctx):
    """处理标签语句，修复非CONTINUE终止"""
    m = re.match(r'^(\s*)(\d+)(\s+)(\S.*)$', line)
    if not m:
        return False
    indent, label, sp, stmt = m.groups()
    # 插入pending的CONTINUE
    for nl, ol in list(ctx['pending'].items()):
        if ol == label:
            ctx['result'].append(f'   {nl} CONTINUE\n')
            del ctx['pending'][nl]
            if nl in ctx['do_stack']:
                ctx['do_stack'].remove(nl)
    # 非CONTINUE终止转为CONTINUE
    if label in ctx['do_stack'] and not stmt.strip().upper().startswith('CONTINUE'):
        ctx['result'].append(f'{indent}  {sp}{stmt}' + ('' if stmt.endswith('\n') else '\n'))
        ctx['result'].append(f'   {label} CONTINUE\n')
    else:
        ctx['result'].append(line)
    while label in ctx['do_stack']:
        ctx['do_stack'].remove(label)
    return True


# ============ 第三阶段：后处理 ============

def post_comment_after_end(lines):
    """将最后一个END后的内容转为注释"""
    last_end = max((i for i, l in enumerate(lines) if re.match(r'^\s*END\s*$', l, re.I)), default=-1)
    for i in range(last_end + 1, len(lines)):
        if lines[i].strip() and not is_comment(lines[i]):
            lines[i] = '! ' + lines[i]
    return lines


def post_file_header(lines):
    """文件头非代码行转为注释"""
    for i in range(min(3, len(lines))):
        if lines[i].strip() and not lines[i].lstrip().startswith(('!', 'IMPLICIT', 'PROGRAM')):
            lines[i] = '! ' + lines[i]
    return lines


# ============ 主处理流程 ============

def process(inf, outf):
    """主处理函数"""
    with open(inf, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # === 第一阶段：符号标准化 ===
    lines = [normalize_linenum(l) for l in lines]   # 移除行号前缀
    lines = [normalize_tab(l) for l in lines]       # TAB展开为空格
    lines = [normalize_spaces(l) for l in lines]    # 标准化空格（DO/指数/GOTO）
    lines = [normalize_indent(l) for l in lines]    # 标准化缩进和标签
    lines = [normalize_keywords(l) for l in lines]  # 关键字转大写
    
    # === 第二阶段：语法转换 ===
    lines = [conv_comment(l) for l in lines]        # 注释符号转换
    lines = [conv_type(l) for l in lines]           # 数据类型转换
    lines = [conv_hollerith(l) for l in lines]      # Hollerith转字符串
    lines = [conv_format(l) for l in lines]         # FORMAT语句修复
    lines = [conv_pause(l) for l in lines]          # PAUSE转PRINT
    lines = [conv_end(l) for l in lines]            # END语句标准化
    lines = conv_arith_if(lines)                    # 算术IF转IF-GOTO
    lines = conv_continuation(lines)                # 续行处理
    lines = fix_do_loops(lines)                     # 修复DO循环结构
    
    # === 第三阶段：后处理 ===
    lines = post_comment_after_end(lines)           # END后内容注释化
    lines = post_file_header(lines)                 # 文件头注释化
    
    with open(outf, 'w', encoding='utf-8') as f:
        f.writelines(lines)
    print(f'转换完成: {inf} -> {outf}')


if __name__ == '__main__':
    i = sys.argv[1] if len(sys.argv) > 1 else 'adkm.f77'
    o = sys.argv[2] if len(sys.argv) > 2 else i.replace('.f77', '.f90')
    process(i, o)
