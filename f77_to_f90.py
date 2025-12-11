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


# ============ 第一阶段：符号标准化 ============

def normalize_linenum(line):
    """移除行号前缀"""
    return re.sub(r'^\s*\d+→', '', line)


def normalize_tab(line):
    """将TAB展开为空格"""
    if '\t' not in line:
        return line
    result, col = [], 0
    for c in line:
        if c == '\t':
            spaces = 6 - col if col < 6 else 1
            result.append(' ' * spaces)
            col = 6 if col < 6 else col + 1
        else:
            result.append(c)
            col += 1
    return ''.join(result)


def normalize_spaces(line):
    """标准化空格：DO/IF语句、指数格式、GO TO"""
    if line.lstrip().startswith('!'):
        return line
    # DO104N → DO 104 N
    line = re.sub(r'\bDO\s*(\d+)([A-Z])', r'DO \1 \2', line, flags=re.I)
    # 0.1D 01 → 0.1D01
    line = re.sub(r'(\d)[DE]\s+(\d)', r'\1D\2', line, flags=re.I)
    # GO TO 标准化
    line = re.sub(r'\bGOTO\b', 'GO TO', line, flags=re.I)
    return line


def normalize_indent(line):
    """标准化缩进和标签格式"""
    if not line.strip() or line.lstrip().startswith('!'):
        return line
    stripped = line.lstrip()
    leading = len(line) - len(stripped)
    # 标签行：数字+空格+代码
    m = re.match(r'^(\d+)(\s+)(\S.*)$', stripped)
    if m:
        label, _, rest = m.groups()
        nl = '\n' if line.endswith('\n') else ''
        return f'   {label} {rest.rstrip()}' + nl
    # 过深缩进标准化为6列
    if leading > 10:
        nl = '\n' if line.endswith('\n') else ''
        return '      ' + stripped.rstrip() + nl
    return line


def normalize_keywords(line):
    """关键字转为大写"""
    if line.lstrip().startswith('!'):
        return line
    kws = ['function', 'subroutine', 'implicit', 'do', 'end', 'if', 'then',
           'else', 'endif', 'return', 'go to', 'call', 'continue', 'format',
           'write', 'read', 'data', 'dimension', 'common', 'parameter', 'entry']
    for kw in kws:
        line = re.sub(r'\b' + kw + r'\b', kw.upper(), line, flags=re.I)
    return line


# ============ 第二阶段：语法转换 ============

def conv_comment(line):
    """第1列C/c/*/!转为注释"""
    if line and line[0] in 'Cc*!':
        return '!' + line[1:]
    return line


def conv_type(line):
    """转换数据类型 REAL*8/COMPLEX*16"""
    line = re.sub(r'\bREAL\*8\b', 'REAL(8)', line, flags=re.I)
    line = re.sub(r'\bCOMPLEX\*16\b', 'COMPLEX(8)', line, flags=re.I)
    line = re.sub(r'(IMPLICIT\s+)REAL\*8', r'\1REAL(8)', line, flags=re.I)
    return line


def conv_hollerith(line):
    """转换Hollerith格式nHxxx为字符串"""
    result, i = [], 0
    while i < len(line):
        m = re.match(r'(\d+)H', line[i:])
        if m:
            n = int(m.group(1))
            start = i + len(m.group(0))
            if start + n <= len(line):
                text = line[start:start+n].replace("'", "''")
                result.append("'" + text + "'")
                i = start + n
                continue
        result.append(line[i])
        i += 1
    return ''.join(result)


def conv_format(line):
    """修复FORMAT语句问题"""
    return re.sub(r'(FORMAT\s*\()\s*,', r'\1', line, flags=re.I)


def conv_pause(line):
    """PAUSE转为PRINT"""
    return re.sub(r'\bPAUSE\b', 'PRINT *,', line, flags=re.I)


def conv_end(line):
    """标准化END语句"""
    if re.match(r'^\s*end\s*$', line, re.I):
        return '      END\n'
    return line


def conv_arith_if(lines):
    """算术IF转换为IF-GOTO结构"""
    result = []
    for line in lines:
        m = re.match(r'^(\s*)(\d*\s*)IF\s*\((.+)\)\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*$', line, re.I)
        if m:
            ind, lab, expr, l1, l2, l3 = m.groups()
            base = ind + lab
            if l1 == l2 == l3:
                result.append(f"{base}GO TO {l1}\n")
            elif l1 == l2:
                result.append(f"{base}IF (({expr}) .GT. 0) GO TO {l3}\n")
                result.append(f"{ind}      GO TO {l1}\n")
            elif l2 == l3:
                result.append(f"{base}IF (({expr}) .LT. 0) GO TO {l1}\n")
                result.append(f"{ind}      GO TO {l2}\n")
            elif l1 == l3:
                result.append(f"{base}IF (({expr}) .EQ. 0) GO TO {l2}\n")
                result.append(f"{ind}      GO TO {l1}\n")
            else:
                result.append(f"{base}IF (({expr}) .LT. 0) GO TO {l1}\n")
                result.append(f"{ind}      IF (({expr}) .EQ. 0) GO TO {l2}\n")
                result.append(f"{ind}      GO TO {l3}\n")
        else:
            result.append(line)
    return result


def conv_continuation(lines):
    """处理续行：第1列数字或第6列非空字符"""
    result = []
    for line in lines:
        if line.lstrip().startswith('!'):
            result.append(line)
        elif line and line[0].isdigit():
            # 第1列数字表示续行
            content = line[1:].lstrip()
            if content.strip() and result and result[-1].strip():
                result[-1] = result[-1].rstrip('\n') + ' &\n'
                result.append('      ' + content)
        elif len(line) > 5 and line[5] not in (' ', '0', '\n'):
            # 第6列非空表示续行
            label = line[:5].strip()
            if label and label.isdigit():
                result.append(line)  # 标签行保持原样
            else:
                if result and result[-1].strip():
                    result[-1] = result[-1].rstrip('\n') + ' &\n'
                result.append('      ' + line[6:])
        else:
            result.append(line)
    return result


# ============ 第三阶段：后处理 ============

def post_comment_after_end(lines):
    """将最后一个END后的内容转为注释"""
    last_end = -1
    for i, line in enumerate(lines):
        if re.match(r'^\s*END\s*$', line, re.I):
            last_end = i
    if last_end >= 0:
        for i in range(last_end + 1, len(lines)):
            if lines[i].strip() and not lines[i].lstrip().startswith('!'):
                lines[i] = '! ' + lines[i]
    return lines


def post_file_header(lines):
    """文件头非代码行转为注释"""
    if not lines:
        return lines
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
    lines = conv_continuation(lines)               # 续行处理
    
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
