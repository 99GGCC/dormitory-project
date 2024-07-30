package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.StudentDTO;
import com.dormitory.controller.qry.StudentQry;
import com.dormitory.controller.vo.StudentVO;
import com.dormitory.service.SysStudentService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 学生信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/student")
@Api(value = "AdminStudentController", tags = {"学生信息接口"})
public class AdminStudentController {

    /**
     * 学生信息Service
     */
    private final SysStudentService sysStudentService;

    @ApiOperation("学生信息分页查询")
    @GetMapping("/page")
    public R<IPage<StudentVO>> pageByQry(StudentQry qry) {
        return R.success(sysStudentService.pageByQry(qry));
    }

    @ApiOperation("学生信息详情")
    @GetMapping("/detail/{studentId}")
    public R<StudentVO> detail(@PathVariable @NotNull(message = "学生ID") Long studentId) {
        return R.success(sysStudentService.detailById(studentId));
    }

    @ApiOperation("新增学生信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid StudentDTO dto) {
        return R.success(sysStudentService.add(dto));
    }

    @ApiOperation("编辑学生信息")
    @PostMapping("/edit/{studentId}")
    public R<Boolean> edit(@NotNull(message = "学生ID") @PathVariable Long studentId,
                           @RequestBody @Valid StudentDTO dto) {
        return R.success(sysStudentService.edit(studentId, dto));
    }

    @ApiOperation("设置学生状态")
    @PostMapping("/status/{studentId}")
    public R<Boolean> status(@NotNull(message = "学生ID") @PathVariable Long studentId,
                             @RequestParam Integer status) {
        return R.success(sysStudentService.status(studentId, status));
    }

    @ApiOperation("删除学生信息")
    @PostMapping("/del/{studentId}")
    public R<Boolean> del(@NotNull(message = "学生ID") @PathVariable Long studentId) {
        return R.success(sysStudentService.del(studentId));
    }
}

