package com.dormitory.controller.web.student;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.qry.SignInRecordQry;
import com.dormitory.controller.vo.SignInRecordVO;
import com.dormitory.service.SignInRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 考勤记录表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/student/sign/in/record")
@Api(value = "StudentSignInRecordController", tags = {"考勤记录接口"})
public class StudentSignInRecordController {

    /**
     * 考勤记录Service
     */
    private final SignInRecordService signInRecordService;

    @ApiOperation("考勤记录分页查询")
    @PostMapping("/page")
    public R<IPage<SignInRecordVO>> pageByQry(@RequestBody SignInRecordQry qry) {
        // 设置考勤记录
        qry.setStudentId(StpStudentUtil.getLoginIdAsLong());
        // 调用分页接口查询考勤记录
        return R.success(signInRecordService.pageByQry(qry));
    }

    @ApiOperation("考勤记录详情")
    @GetMapping("/detail/{recordId}")
    public R<SignInRecordVO> detail(@PathVariable @NotNull(message = "记录ID") Long recordId) {
        return R.success(signInRecordService.detailById(recordId));
    }

    @ApiOperation("学生签到")
    @PostMapping("/sign/{recordId}")
    public R<Boolean> studentSign(@NotNull(message = "记录ID") @PathVariable Long recordId) {
        return R.success(signInRecordService.studentSign(recordId));
    }
}

